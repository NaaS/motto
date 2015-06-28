(*
   Type inference for Crisp expressions.
   Nik Sultana, Cambridge University Computer Lab, June 2015
*)

open Crisp_syntax
open Crisp_syntax_aux
open State

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
    let _ = if strict then
      if List.exists undefined_ty tys then
        failwith "Tuple contained an undefined type" in
    (Tuple (None, tys), st)

  | Seq (e1, e2) ->
    let e1_ty, st' = ty_of_expr ~strict st e1 in
    let _ = if strict then
      if undefined_ty e1_ty then
        failwith "Cannot have undefined type" in
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
    let _ = if strict then
      if undefined_ty (fst ans) then
        failwith "Cannot have undefined type" in
    ans

  | Str _ -> (String (None, []), st)

  | LocalDef ((value_name, type_value_opt), e) ->
    let ty =
      let e_ty, _ = ty_of_expr ~strict st e in
      match type_value_opt with
      | None ->
        (*We MUST be able to infer the type of e, either from the type
          annotation, or from e itself. If e's type is Undefined, and no
          annotation is given, then complain.*)
        if e_ty = Undefined then
          failwith "Expression cannot be given ground type"
        else e_ty
      | Some ty_value ->
        if e_ty = Undefined then
          ty_value
        else if e_ty <> ty_value then
          failwith "Expression cannot be given ground type"
        else e_ty in
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
    let _ = if strict then assert (ty = Undefined || expected_ty = ty) in
    (expected_ty, st)

  | IntegerRange (_, _) ->
    (List (None, Integer (None, []), None, []), st)

  | Iterate (label, range_e, acc_opt, body_e, unordered) ->
    let st', acc_opt_ty =
      match acc_opt with
      | None -> st, None
      | Some (acc_label, acc_e) ->
        let ty, _ = ty_of_expr ~strict st acc_e in
        assert (ty <> Undefined);
        let _, st' =
          Naasty_aux.extend_scope_unsafe (Term Value) st ~src_ty_opt:(Some ty) acc_label in
        (st', Some ty) in
    let st'' =
      let cursor_ty =
        match fst (ty_of_expr ~strict st' range_e) with
        | List (_, ty', _, _) ->
          assert (ty' <> Undefined);
          ty'
        | _ -> failwith "Was expecting list type" in
      let _, st'' = Naasty_aux.extend_scope_unsafe (Term Value) st'
                       ~src_ty_opt:(Some cursor_ty) label in
      st'' in
    let ty, _ = ty_of_expr ~strict st'' body_e in
    let _ =
      if strict then
        assert (acc_opt_ty = None || acc_opt_ty = Some ty) in
    (*NOTE we should return st, not st'', since we don't want the bindings made
           for body_e to spill over to the rest of the scope.*)
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

  (*value_name[idx] := expression*)
  | UpdateIndexable (map_name, idx_e, body_e) ->
    let ty, _ = ty_of_expr ~strict st body_e in
    let md =
      match lookup_term_data (Term Map_Name) st.term_symbols map_name with
      | None -> failwith ("Missing declaration for map name " ^ map_name)
      | Some (_, md) -> md in
    let idx_ty, _ = ty_of_expr ~strict st idx_e in
    let expected_idx_ty, value_ty =
      match md.source_type with
      | Some (Dictionary (lbl_opt, idx_ty, val_ty)) ->
        assert (lbl_opt = Some map_name);
        idx_ty, val_ty
      | _ -> failwith "Expected to find dictionary type" in
    let _ =
      if strict then
        assert (value_ty = ty);
        assert (expected_idx_ty = idx_ty) in
    (ty, st)
  (*value_name[idx]*)
  | IndexableProjection (map_name, idx_e) ->
    let md =
      match lookup_term_data (Term Map_Name) st.term_symbols map_name with
      | None -> failwith ("Missing declaration for map name " ^ map_name)
      | Some (_, md) -> md in
    let idx_ty, _ = ty_of_expr ~strict st idx_e in
    let expected_idx_ty, value_ty =
      match md.source_type with
      | Some (Dictionary (lbl_opt, idx_ty, val_ty)) ->
        assert (lbl_opt = Some map_name);
        idx_ty, val_ty
      | _ -> failwith "Expected to find dictionary type" in
    let _ =
      if strict then
        assert (expected_idx_ty = idx_ty) in
    (value_ty, st)

  | Record fields ->
    let (field_tys, (record_tys, labels)) =
      List.fold_right (fun (label, e) acc ->
        let ty, _ = ty_of_expr ~strict st e in
        assert (ty <> Undefined);
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
              assert (ty' <> Undefined);
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
              | Some lbl ->
                lbl = label &&
                (field_ty = ty || field_ty = Undefined)) field_tys in
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
  | Functor_App (functor_name, fun_args) ->
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
                  if tv <> ty then
                    failwith "Incorrect return type for disjunct" (*FIXME give more info*)
                | _ ->
                  failwith "Incorrect identifier kind for functor" (*FIXME give more info*) in
            ty
          | _ -> failwith ("Functor's return type is invalid, returns more than one value: " ^ functor_name) in
        assert (ret_ty <> Undefined);
        let _ =
          if strict then
            (*Canonicalise the function's arguments -- eliminating any named-parameter
              occurrences.*)
            let arg_expressions =
              Crisp_syntax_aux.order_fun_args functor_name st fun_args in
            let fun_args_tys =
              List.map (ty_of_expr ~strict st) arg_expressions
              |> List.map fst in
            List.iter (fun (ty1, ty2) ->
              if ty1 <> ty2 &&
                 ty1 <> Undefined &&
                 ty2 <> Undefined then
                failwith "Wrong-typed parameter to functor")(*FIXME give more info*)
            (List.combine fun_args_tys arg_tys) in
        (ret_ty, st)
      | Some _ ->
        failwith ("Function types currently carried in a different field in the symbol table")
    end

  | CaseOf (e, cases) ->
    let ty, _ = ty_of_expr ~strict st e in
    (*ty must be a Disjoint_Union*)
    let expected_disjuncts =
      match ty with
      | Disjoint_Union (_, tys) -> tys
      | _ -> failwith "Was expecting disjoint union" (*FIXME give more info*) in
    let expected_disjunct_heads =
      List.map (fun ty ->
        match label_of_type ty with
        | None -> failwith "Expected type to be labelled"(*FIXME give more info*)
        | Some lbl -> lbl) expected_disjuncts in
    (*check that each disjunct was registered with the symbol table, and has the
      right identifier_kind*)
    let _ =
      if strict then
         List.iter (fun label ->
           match lookup_term_data (Term Undetermined) st.term_symbols label with
           | None -> failwith ("Missing declaration for '" ^ label ^ "'")
           | Some (_, {identifier_kind; _}) ->
             if identifier_kind <> Disjunct ty then
               failwith ("Disjunct " ^ label ^ " has incorrect identifier kind")
        ) expected_disjunct_heads (*FIXME give more info*) in
    (*within cases, the head must be a Functor_App, a disjunct of ty.*)
    let actual_disjuncts, body_tys =
      List.map (fun (head_e, body_e) ->
        let head_label,
            (*extention to type environment, containing typing of variables used
              in pattern matching; using this we can type the body_e*)
            arg_vars =
          match head_e with
          | Functor_App (functor_name, fun_args) ->
            (*expression matching*)
            (*NOTE for the time being i make the following simplifications:
              1. all fun_args are Exps -- there's to be no parameter naming
              2. all are Vars -- to avoid having to implement a coverage checker
              for the time being.*)
            (functor_name,
             List.map (fun arg ->
               match arg with
               | Exp (Variable label) -> label
               | _ -> failwith "Invalid disjunct head"(*FIXME give more info*))
               fun_args)
          | _ -> failwith "Disjunct heads must be functors"(*FIXME give more info*) in
        let expected_arg_tys =
          match lookup_function_type st head_label with
          | None -> failwith ("Missing declaration for functor " ^ head_label)
          | Some f_ty ->
            let ((chans, arg_tys), ret_tys) = extract_function_types f_ty in
            (*disjuncts cannot be passed channels!*)
            assert (chans = []);
            arg_tys in
        let _ =
          if List.length expected_arg_tys <> List.length arg_vars then
            failwith "Disjunct has wrong number of parameters" (*FIXME give more info*) in
        let st' =
          List.fold_right (fun (name, ty) st ->
            let _, st' =
              Naasty_aux.extend_scope_unsafe (Term Value) st ~src_ty_opt:(Some ty) name
            in st') (List.combine arg_vars expected_arg_tys) st in
        let body_ty, _ = ty_of_expr ~strict st' body_e in
        (head_label, body_ty)) cases
      |> List.split in
    (*all the disjuncts of the Disjoint_Union must be mentioned in the heads*)
    let _ =
      if strict then
        List.iter (fun label ->
          if not (List.exists (fun lbl -> label = lbl) expected_disjunct_heads) then
            failwith ("Extra disjunct -- this had not been mentioned in the type specification: " ^ label))
         actual_disjuncts in
    (*each body must be of the same type, and is the result type of the whole expression*)
    assert (List.length body_tys > 0);
    let ty =
      List.fold_right (fun ty acc ->
        if ty <> acc then
          failwith "Bodies don't all have the same type"
        else acc) (List.tl body_tys) (List.hd body_tys) in
    (ty, st)

  | EmptyList -> (Undefined, st)
  | ConsList (h_e, t_e) ->
    let h_ty, _ = ty_of_expr ~strict st h_e in
    assert (h_ty <> Undefined);
    let t_ty, _ = ty_of_expr ~strict st t_e in
    let ty =
      match t_ty with
      | List (_, ty, _, _) as list_ty ->
        if ty <> Undefined then
          begin
          assert (ty = h_ty);
          list_ty
          end
        else List (None, h_ty, None, [])
      | _ -> failwith "Tail must be of list type" in
    (ty, st)
  | AppendList (l1, l2) ->
    let l1_ty, _ = ty_of_expr ~strict st l1 in
    let l2_ty, _ = ty_of_expr ~strict st l2 in
    assert (l1_ty = l2_ty || l1_ty = Undefined || l2_ty = Undefined);
    (l1_ty, st)

  | Send (chan_e, data_e) ->
    let data_ty, _ = ty_of_expr ~strict st data_e in
    assert (data_ty <> Undefined);
    let chan_ty, _ = ty_of_expr ~strict st chan_e in
    let ty =
      match chan_ty with
      | ChanType ct ->
        if tx_chan_type ct = data_ty then
          data_ty
        else failwith "Mismatch between type of data and that of channel" (*FIXME give more info*)
      | _ -> failwith "Expected type to be channel" (*FIXME give more info*) in
    (ty, st)
  | Receive (chan_e, data_e) ->
    let data_ty, _ = ty_of_expr ~strict st data_e in
    assert (data_ty <> Undefined);
    let chan_ty, _ = ty_of_expr ~strict st chan_e in
    let ty =
      match chan_ty with
      | ChanType ct ->
        if rx_chan_type ct = data_ty then
          data_ty
        else failwith "Mismatch between type of data and that of channel" (*FIXME give more info*)
      | _ -> failwith "Expected type to be channel" (*FIXME give more info*) in
    (ty, st)
  | Exchange (chan1_e, chan2_e) ->
    let chan1_ty, _ = ty_of_expr ~strict st chan1_e in
    let chan2_ty, _ = ty_of_expr ~strict st chan2_e in
    let ty =
      match chan1_ty, chan2_ty with
      | ChanType ct1, ChanType ct2 ->
        if rx_chan_type ct1 = rx_chan_type ct2 &&
          tx_chan_type ct1 = tx_chan_type ct2 then
          flick_unit_type
        else failwith "Mismatch between type of data and that of channel" (*FIXME give more info*)
      | _ -> failwith "Expected both types to be channels" (*FIXME give more info*) in
    (ty, st)

  (*NOTE currently we don't support dependently-typed lists*)
  | _ -> failwith ("TODO")
