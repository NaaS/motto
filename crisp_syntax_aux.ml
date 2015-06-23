(*
   Supporting functions for the Crisp syntax definition.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open Crisp_syntax

let name_of_type = function
  | Type ty_decl -> ty_decl.type_name
  | _ -> failwith "Expected type declaration."

(*Unwraps a Crisp function type into a tuple of its components*)
let extract_function_types (FunType (FunDomType (chans, arg_tys), FunRetType ret_tys)) =
  ((chans, arg_tys), ret_tys)

(*Sets the label of a type unless it's already defined. This is used to bridge
  the gap between Flick and NaaSty, since the latter expects all type
  declarations to be associated with their names, while the former distributes
  this a bit (the name is stored in a type record).*)
let update_empty_label label (ty : type_value) =
  match ty with
  | UserDefinedType (label_opt, type_name) ->
    if label_opt = None then
      UserDefinedType (Some label, type_name)
    else failwith "Cannot set an already-set label"
  | String (label_opt, type_annotation) ->
    if label_opt = None then
      String (Some label, type_annotation)
    else failwith "Cannot set an already-set label"
  | Integer (label_opt, type_annotation) ->
    if label_opt = None then
      Integer (Some label, type_annotation)
    else failwith "Cannot set an already-set label"
  | Boolean (label_opt, type_annotation) ->
    if label_opt = None then
      Boolean (Some label, type_annotation)
    else failwith "Cannot set an already-set label"
  | RecordType (label_opt, tys, type_annotation) ->
    if label_opt = None then
      RecordType (Some label, tys, type_annotation)
    else failwith "Cannot set an already-set label"
  | Disjoint_Union (label_opt, tys) ->
    if label_opt = None then
      Disjoint_Union (Some label, tys)
    else failwith "Cannot set an already-set label"
  | List (label_opt, ty, dep_idx_opt, type_annotation) ->
    if label_opt = None then
      List (Some label, ty, dep_idx_opt, type_annotation)
    else failwith "Cannot set an already-set label"
  | Empty -> Empty
  | IPv4Address label_opt ->
    if label_opt = None then
      IPv4Address (Some label)
    else failwith "Cannot set an already-set label"
  | Tuple (label_opt, tys) ->
    if label_opt = None then
      Tuple (Some label, tys)
    else failwith "Cannot set an already-set label"
  | Dictionary (label_opt, ty) ->
    if label_opt = None then
      Dictionary (Some label, ty)
    else failwith "Cannot set an already-set label"
  | Reference (label_opt, ty) ->
    if label_opt = None then
      Reference (Some label, ty)
    else failwith "Cannot set an already-set label"

(*Assuming that the given declaration is a type declaration, this function
  extracts the type being declared.*)
let the_ty_of_decl = function
  | Type ty_decl -> ty_decl.type_value
  | _ -> failwith "Was expecting a type declaration."


type ty_env = (string * (type_value list * type_value)) list
let extend_env env x =  x :: env
let decompose_container (ty : type_value) : type_value list =
  match ty with
  | List (_, ty', _, _) -> [ty']
  | Dictionary (_, ty') -> [ty']
  | Tuple (_, tys') -> tys'
  | Reference (_, ty') -> [ty']
  | RecordType (_, tys', _) -> tys'
  | Disjoint_Union (_, tys') -> tys'
  | _ -> failwith "Type is not a container"
let label_of_type : type_value -> label option = function
  | UserDefinedType (l_opt, _)
  | String (l_opt, _)
  | Integer (l_opt, _)
  | Boolean (l_opt, _)
  | Tuple (l_opt, _)
  | Dictionary (l_opt, _)
  | Reference (l_opt, _)
  | Disjoint_Union (l_opt, _) -> l_opt
  | RecordType (l_opt, _, _) -> l_opt
  | List (l_opt, _, _, _) -> l_opt
  | Empty -> failwith "Empty type cannot be given a label"
  | IPv4Address l_opt
  | Alpha l_opt -> l_opt

let rec ty_of_expr ?strict:(strict : bool = false) (env : ty_env) : expression -> type_value * ty_env = function
  | Variable label ->
    let env_extension = [] in
    begin
      try
        let args, res = List.assoc label env in
        if args = [] then res, env_extension
        else failwith "Variables cannot have function type"
      with Not_found ->
        failwith ("Missing declaration for '" ^ label ^ "'")
    end

  (*Boolean expressions*)
  | True
  | False -> (Boolean (None, []), [])
  | And (e1, e2)
  | Or (e1, e2) ->
    let ans = (Boolean (None, []), []) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict env in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty);
        assert (e1_ty = ans) in
    ans
  | Not e ->
    let ans = (Boolean (None, []), []) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict env e in
        assert (e_ty = ans) in
    ans

  (*Definable over arbitrary types of expressions*)
  | Equals (e1, e2) ->
    let ans = (Boolean (None, []), []) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict env in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty) in
    ans

  (*Definable over arithmetic expressions*)
  | GreaterThan (e1, e2)
  | LessThan (e1, e2) ->
    let ans = (Boolean (None, []), []) in
    let expected = (Integer (None, []), []) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict env in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty);
        assert (e1_ty = expected) in
    ans

  (*Arithmetic expressions*)
  | Int _ -> (Integer (None, []), [])

  (*NOTE for these expressions we might want to look deeper, to differentiate
    between different kinds of numbers -- ints, floats, etc*)
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2) ->
    let ans = (Integer (None, []), []) in
    let _ =
      if strict then
        let f = ty_of_expr ~strict env in
        let (e1_ty, e2_ty) = f e1, f e2 in
        assert (e1_ty = e2_ty);
        assert (e1_ty = ans) in
    ans
  | Abs e ->
    let ans = (Integer (None, []), []) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict env e in
        assert (e_ty = ans) in
    ans

  (*Native representation of an IPv4 address*)
  | IPv4_address (_, _, _, _) -> (IPv4Address None, [])
  (*Integer to IP address*)
  | Int_to_address e ->
    let ans = (IPv4Address None, []) in
    let expected = (Integer (None, []), []) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict env e in
        assert (e_ty = expected) in
    ans
  (*IP address to integer*)
  | Address_to_int e ->
    let ans = (Integer (None, []), []) in
    let expected = (IPv4Address None, []) in
    let _ =
      if strict then
        let e_ty = ty_of_expr ~strict env e in
        assert (e_ty = expected) in
    ans

  | TupleValue es ->
    let tys =
      List.map (ty_of_expr ~strict env) es
      |> List.map fst in
    (Tuple (None, tys), [])

  | Seq (e1, e2) ->
    let _, env' = ty_of_expr ~strict env e1 in
    ty_of_expr ~strict env' e2

  | ITE (b_exp, e1, e2_opt) ->
    let f = ty_of_expr ~strict env in
    let ans = f e1 in
    let _ =
      if strict then
        begin
          assert (f b_exp = (Boolean (None, []), []));
          match e2_opt with
          | None -> ()
          | Some e2 ->
            assert (ans = f e2);
        end in
    ans

  | Str _ -> (String (None, []), [])

  | LocalDef ((value_name, type_value_opt), e) ->
    let ty, _ = ty_of_expr ~strict env e in
    let _ =
      match type_value_opt with
      | None -> ()
      | Some ty_value -> assert (ty = ty_value) in
    (ty, [value_name, ([], ty)])
  | Update (value_name, e) ->
    let expected_ty =
      let args, res = List.assoc value_name env in
      if args = [] then res
      else failwith "Not expecting a function type" in
    let ty, _ = ty_of_expr ~strict env e in
    let _ = if strict then assert (expected_ty = ty) in
    (ty, [])

  | IntegerRange (_, _) ->
    (List (None, Integer (None, []), None, []), [])

  | Iterate (label, range_e, acc_opt, body_e, unordered) ->
    let env', acc_opt_ty =
      match acc_opt with
      | None -> env, None
      | Some (acc_label, acc_e) ->
        let ty, _ = ty_of_expr ~strict env acc_e in
        (extend_env env (acc_label, ([], ty)), Some ty) in
    let env'' =
      let cursor_ty =
        match fst (ty_of_expr ~strict env range_e) with
        | List (_, ty', _, _) -> ty'
        | _ -> failwith "Was expecting list type" in
      extend_env env' (label, ([], cursor_ty)) in
    (*FIXME if strict, match the type of acc_e with that of body_e.
            NOTE need to use matching not equality, since might
                 have type variables*)
    let ty, _ = ty_of_expr ~strict env'' body_e in
    (ty, [])

  | Map (label, src_e, body_e, unordered) ->
    let env' =
      let cursor_ty =
        match fst (ty_of_expr ~strict env src_e) with
        | List (_, ty', _, _) -> ty'
        | _ -> failwith "Was expecting list type" in
      extend_env env (label, ([], cursor_ty)) in
    let ty, _ = ty_of_expr ~strict env' body_e in
    let _ =
      if strict then
        match ty with
        | List (_, _, _, _) -> ()
        | _ -> failwith "Was expecting list type" in
    (ty, [])

  (*value_name[idx] := expression*)
  | UpdateIndexable (map_name, idx_e, body_e) ->
    let ty, _ = ty_of_expr ~strict env body_e in
    let _ =
      if strict then
        let idx_ty, _ = ty_of_expr ~strict env idx_e in
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
        let idx_ty, _ = ty_of_expr ~strict env idx_e in
        match map_args with
        | [map_idx_ty] ->
          (*NOTE could use stronger relation than equality in order to
                 support record subtyping of index types, say.*)
          if map_idx_ty = idx_ty then ()
          else failwith "Unexpected index type"
        | _ -> failwith "Unexpected map type" in
    (map_res_ty, [])

  | Record fields ->
    let labels, idx_ty =
      List.map (fun (label, e) ->
        let ty, _ = ty_of_expr ~strict env e in
        (label, ty)) fields
      |> List.split in
    (*FIXME check labels if strict*)
    (RecordType (None, idx_ty, []), [])
  | RecordUpdate (e, (label, body_e)) ->
    let ty, _ = ty_of_expr ~strict env e in
    let _ =
      if strict then
        let idx_ty, _ =
          (*FIXME check if body_e's type matches that of label*)
          ty_of_expr ~strict env body_e in
        () in
    (ty, [])
  | RecordProjection (e, label) ->
    let e_ty, _ = ty_of_expr ~strict env e in
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
    (l_ty, [])

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
