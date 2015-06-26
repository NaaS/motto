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
    let labels, idx_ty =
      List.map (fun (label, e) ->
        let ty, _ = ty_of_expr ~strict st e in
        (label, ty)) fields
      |> List.split in
    (*FIXME check labels if strict*)
    (RecordType (None, idx_ty, []), st)
  | RecordUpdate (e, (label, body_e)) ->
    let ty, _ = ty_of_expr ~strict st e in
    let _ =
      if strict then
        let idx_ty, _ =
          (*FIXME check if body_e's type matches that of label*)
          ty_of_expr ~strict st body_e in
        () in
    (ty, st)
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

  (*NOTE currently we don't support dependently-typed lists*)
  | _ -> failwith ("TODO")

(*
    (*also used to form Coproducts, as well as make function calls*)
  | Function_Call of function_name * fun_arg list

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
