(*
   Type inference for Crisp expressions.
   Nik Sultana, Cambridge University Computer Lab, June 2015
*)

open Crisp_syntax
open Crisp_syntax_aux
open State

(*FIXME need to carry expected type? this can be used to disambiguate
        expressions like "[]"*)
let rec ty_of_expr ?strict:(strict : bool = false) (st : state) : expression ->
  type_value * state = function
  | Variable label ->
    let scope = Term Undetermined in
    begin
    match lookup_term_data scope st.term_symbols label with
    | None -> failwith ("Missing declaration for '" ^ label ^ "'")
    | Some (_, {source_type; _}) ->
      match source_type with
      | None -> failwith ("Missing source type for '" ^ label ^ "'")
      | Some ty -> (ty, st)
    end

  (*Boolean expressions*)
  | True
  | False -> (Boolean (None, []), st)
  | And (e1, e2)
  | Or (e1, e2) ->
    let ans = (Boolean (None, []), st) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict st in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty);
        assert (e1_ty = ans) in
    ans
  | Not e ->
    let ans = (Boolean (None, []), st) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict st e in
        assert (e_ty = ans) in
    ans

  (*Definable over arbitrary types of expressions*)
  | Equals (e1, e2) ->
    let ans = (Boolean (None, []), st) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict st in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty) in
    ans

  (*Definable over arithmetic expressions*)
  | GreaterThan (e1, e2)
  | LessThan (e1, e2) ->
    let ans = (Boolean (None, []), st) in
    let expected = (Integer (None, []), st) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict st in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty);
        assert (e1_ty = expected) in
    ans

  (*Arithmetic expressions*)
  | Int _ -> (Integer (None, []), st)

  (*NOTE for these expressions we might want to look deeper, to differentiate
    between different kinds of numbers -- ints, floats, etc*)
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2) ->
    let ans = (Integer (None, []), st) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict st in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty);
        assert (e1_ty = ans) in
    ans
  | Abs e ->
    let ans = (Integer (None, []), st) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict st e in
        assert (e_ty = ans) in
    ans

  (*Native representation of an IPv4 address*)
  | IPv4_address (_, _, _, _) -> (IPv4Address None, st)
  (*Integer to IP address*)
  | Int_to_address e ->
    let ans = (IPv4Address None, st) in
    let expected = (Integer (None, []), st) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict st e in
        assert (e_ty = expected) in
    ans
  (*IP address to integer*)
  | Address_to_int e ->
    let ans = (Integer (None, []), st) in
    let expected = (IPv4Address None, st) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict st e in
        assert (e_ty = expected) in
    ans

  | TupleValue es ->
    let tys =
      List.map (ty_of_expr ~strict st) es
      |> List.map fst in
    (Tuple (None, tys), st)

  | Seq (e1, e2) ->
    let _, st' = ty_of_expr ~strict st e1 in
    ty_of_expr ~strict st' e2

  | ITE (b_exp, e1, e2_opt) ->
    let f = ty_of_expr ~strict st in
    let ans = f e1 in
    let _ =
      if strict then
        begin
          assert (f b_exp = (Boolean (None, []), st));
          match e2_opt with
          | None -> ()
          | Some e2 ->
            assert (ans = f e2);
        end in
    ans

  | Str _ -> (String (None, []), st)

  | LocalDef ((value_name, type_value_opt), e) ->
    let ty, _ = ty_of_expr ~strict st e in
    let _ =
      match type_value_opt with
      | None -> ()
      | Some ty_value -> assert (ty = ty_value) in
    let _, st' =
      Naasty_aux.extend_scope_unsafe (Term Value) st ~src_ty_opt:(Some ty) value_name in
    (ty, st')
  | Update (value_name, e) ->
    let scope = Term Undetermined in
    let expected_ty =
    match lookup_term_data scope st.term_symbols value_name with
    | None -> failwith ("Missing declaration for '" ^ value_name ^ "'")
    | Some (_, {source_type; _}) ->
      match source_type with
      | None -> failwith ("Missing source type for '" ^ value_name ^ "'")
      | Some ty -> ty in
    let ty, _ = ty_of_expr ~strict st e in
    let _ = if strict then assert (expected_ty = ty) in
    (ty, st)

  | IntegerRange (_, _) ->
    (List (None, Integer (None, []), None, []), st)

  | Iterate (label, range_e, acc_opt, body_e, unordered) ->
    let st', acc_opt_ty =
      match acc_opt with
      | None -> st, None
      | Some (acc_label, acc_e) ->
        let ty, _ = ty_of_expr ~strict st acc_e in
        let _, st' = Naasty_aux.extend_scope_unsafe (Term Value) st ~src_ty_opt:(Some ty) acc_label in
        (st', Some ty) in
    let st'' =
      let cursor_ty =
        match fst (ty_of_expr ~strict st' range_e) with
        | List (_, ty', _, _) -> ty'
        | _ -> failwith "Was expecting list type" in
      let _, st'' = Naasty_aux.extend_scope_unsafe (Term Value) st'
                       ~src_ty_opt:(Some cursor_ty) label in
      st'' in
    (*FIXME if strict, match the type of acc_e with that of body_e.
            NOTE need to use matching not equality, since might
                 have type variables*)
    let ty, _ = ty_of_expr ~strict st'' body_e in
    (ty, st)

  | Map (label, src_e, body_e, unordered) ->
    let st' =
      let cursor_ty =
        match fst (ty_of_expr ~strict st src_e) with
        | List (_, ty', _, _) -> ty'
        | _ -> failwith "Was expecting list type" in
      let _, st' = Naasty_aux.extend_scope_unsafe (Term Value) st
                     ~src_ty_opt:(Some cursor_ty) label in
      st' in
    let ty, _ = ty_of_expr ~strict st' body_e in
    let _ =
      if strict then
        match ty with
        | List (_, _, _, _) -> ()
        | _ -> failwith "Was expecting list type" in
    (ty, st)
(*FIXME continue porting
  (*value_name[idx] := expression*)
  | UpdateIndexable (map_name, idx_e, body_e) ->
    let ty, _ = ty_of_expr ~strict st body_e in
    let _ =
      if strict then
        let idx_ty, _ = ty_of_expr ~strict st idx_e in
        let map_args, map_res_ty = List.assoc map_name env in
        let _ =
          match map_args with
          | [map_idx_ty] ->
            (*NOTE could use stronger relation than equality in order to
                   support record subtyping of index types, say.*)
            if map_idx_ty = idx_ty then ()
            else failwith "Unexpected index type"
          | _ -> failwith "Unexpected map type" in
        if map_res_ty = ty then ()
        else failwith "Unexpected result type" in
    (ty, [])
  (*value_name[idx]*)
  | IndexableProjection (map_name, idx_e) ->
    let map_args, map_res_ty = List.assoc map_name env in
    let _ =
      if strict then
        let idx_ty, _ = ty_of_expr ~strict st idx_e in
        match map_args with
        | [map_idx_ty] ->
          (*NOTE could use stronger relation than equality in order to
                 support record subtyping of index types, say.*)
          if map_idx_ty = idx_ty then ()
          else failwith "Unexpected index type"
        | _ -> failwith "Unexpected map type" in
    (map_res_ty, [])
*)

  | Record fields ->
    let (field_tys, (record_tys, labels)) =
      List.fold_right (fun (label, e) acc ->
        let ty, _ = ty_of_expr ~strict st e in
        let md =
          match State.lookup_term_data (Term Undetermined) st.term_symbols label with
          | None -> failwith ("Missing declaration for " ^ label)
          | Some (_, md) -> md in
        let _ =
          (*check if given labels are well-typed*)
          if strict then
            match md.source_type with
            | None -> failwith ("Missing type for " ^ label)
            | Some ty' ->
              if ty <> ty' then
                failwith "Unexpected type"(*FIXME give more info*) in
        let record_ty =
          match md.identifier_kind with
          | Field record_ty -> record_ty
          | _ -> failwith "Unexpected identifier kind"(*FIXME give more info*) in
        (ty, (record_ty, label)) :: acc) fields []
      |> List.split
      |> General.apsnd List.split in
    let _ =
      if strict then
        begin
        assert (record_tys <> []); (*doesn't make sense for record to be empty*)
        let record_ty =
          (*check if all labels relate to the same record type!*)
          List.fold_right (fun ty acc ->
            if ty <> acc then
              failwith "Labels relate to different types"
            else acc) (List.tl record_tys) (List.hd record_tys) in
        (*check if all labels have been given*)
        match record_ty with
        | RecordType (_, field_tys, _) ->
          List.iter (fun ty ->
            match label_of_type ty with
            | None -> failwith "Expected type to be labelled"(*FIXME give more info*)
            | Some label ->
              let label_defined_in_value =
                List.exists (fun lbl -> lbl = label) labels in
              if not label_defined_in_value then
                failwith ("Missing label in record: " ^ label)) field_tys
        | _ -> failwith "Expected record type"(*FIXME give more info*)
        end in
    (RecordType (None, field_tys, []), st)
  | RecordUpdate (e, (label, body_e)) ->
    let record_ty, _ = ty_of_expr ~strict st e in
    let _ =
      if strict then
        let field_ty, _ =
          ty_of_expr ~strict st body_e in
        match record_ty with
        | RecordType (_, field_tys, _) ->
          let field_exists_in_record =
            List.exists (fun ty ->
              match label_of_type ty with
              | None -> failwith "Expected type to be labelled"(*FIXME give more info*)
              | Some lbl -> lbl = label && field_ty = ty) field_tys in
          if not field_exists_in_record then
            failwith ("Label " ^ label ^ " doesn't belong to a field in record")
        | _ -> failwith "Expected record type"(*FIXME give more info*) in
    (record_ty, st)
  | RecordProjection (e, label) ->
    let e_ty, _ = ty_of_expr ~strict st e in
    let l_ty =
      match e_ty with
      | RecordType (_, tys', _) ->
        let filtered_tys =
          List.filter (fun ty' -> label_of_type ty' = Some label) tys' in
        begin
        match filtered_tys with
        | [ty] -> ty
        | _ -> failwith "Zero or several fields had the label sought"
        end
      | _ -> failwith "Was expecting record type" in
    (l_ty, st)

    (*also used to form Coproducts, as well as make function calls*)
  | Function_Call (functor_name, fun_args) ->
    let scope =
      (*scope can be either Term Function_Name or Term Disjunct; this will be
        checked later when we get something back from the symbol table.*)
      Term Undetermined in
    begin
    match lookup_term_data scope st.term_symbols functor_name with
    | None -> failwith ("Missing declaration for '" ^ functor_name ^ "'")
    | Some (_, {source_type; identifier_kind; _}) ->
      match source_type with
      | None ->
        let functor_ty =
          match lookup_function_type st functor_name with
          | None -> failwith ("Missing declaration for functor " ^ functor_name)
          | Some f_ty -> f_ty in
        let ((chans, arg_tys), ret_tys) = extract_function_types functor_ty in
        assert (chans = []);
        let ret_ty =
          match ret_tys with
          | [ty] ->
            let _ =
              if strict then
                match identifier_kind with
                | Function_Name -> ()
                | Disjunct tv ->
                  if tv <> ty then failwith "Incorrect return type for disjunct" (*FIXME give more info*)
                | _ -> failwith "Incorrect identifier kind for functor" (*FIXME give more info*) in
            ty
          | _ -> failwith ("Functor's return type is invalid, returns more than one value: " ^ functor_name) in
        let _ =
          if strict then
            (*Canonicalise the function's arguments -- eliminating any named-parameter
              occurrences.*)
            let arg_expressions = Crisp_syntax_aux.order_fun_args functor_name st fun_args in
            let fun_args_tys =
              List.map (ty_of_expr ~strict st) arg_expressions
              |> List.map fst in
            List.iter (fun (ty1, ty2) ->
              if ty1 <> ty2 then failwith "Wrong-typed parameter to functor")(*FIXME give more info*)
            (List.combine fun_args_tys arg_tys) in
        (ret_ty, st)
      | Some _ ->
        failwith ("Function types currently carried in a different field in the symbol table")
    end

  (*NOTE currently we don't support dependently-typed lists*)
  | _ -> failwith ("TODO")

(*
    (*2nd expr must ben Function_Call -- each of them a disjunct*)
  | CaseOf of expression * (expression * expression) list

  | EmptyList
  | ConsList of expression * expression
  | AppendList of expression * expression

   (*Channel operations. Can be overloaded to, say, send values
    on a channel, or to first obtain values from a channel then send it to
    another.*)
  | Send of expression * expression
  | Receive of expression * expression
  (*Send and receive between two channels*)
  | Exchange of expression * expression
*)
