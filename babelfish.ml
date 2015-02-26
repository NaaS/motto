(*
   Translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty


type state =
  { pragmas : string list;
    type_declarations : naasty_type list;
    next_typesymbol : integer;
    type_symbols : (string * integer) list;
    next_symbol : integer;
    symbols : (string * integer) list;
  }

let initial_state =
  { pragmas = [];
    type_declarations = [];
    next_typesymbol = 1;
    type_symbols = [];
    next_symbol = 1;
    symbols = [];
  }

(*Sets the label of a type unless it's already defined. This is used to bridge
  the gap between Flick and NaaSty, since the latter expects all type
  declarations to be associated with their names, while the former distributes
  this a bit (the name is stored in a type record).*)
let update_empty_label label = function
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

let rec naasty_of_flick_type (st : state) (ty : type_value) : (naasty_type * state) =
  let check_and_resolve_typename type_name =
    if not (List.mem_assoc type_name st.type_symbols) then
      failwith ("Undeclared type: " ^ type_name)
    else List.assoc type_name st.type_symbols in
  let check_and_generate_typename typename_opt =
    match typename_opt with
    | None -> failwith ("Was expecting type name.")
    | Some type_name ->
      if List.mem_assoc type_name st.type_symbols then
        (*shadowing is forbidden*)
        failwith ("Already declared type: " ^ type_name)
      else
        (st.next_symbol,
         { st with
           symbols = (type_name, st.next_symbol) :: st.symbols;
           next_symbol = 1 + st.next_symbol;
         }) in
  let check_and_generate_name label_opt =
    match label_opt with
    | None -> (None, st)
    | Some s ->
      if (List.mem_assoc s st.symbols) then
        (*shadowing is forbidden*)
        failwith ("Already declared: " ^ s);
      (Some st.next_symbol,
       { st with
         symbols = (s, st.next_symbol) :: st.symbols;
         next_symbol = 1 + st.next_symbol;
       }) in
  match ty with
  | Disjoint_Union (_, _) -> failwith "Unsupported"
  | List (_, _, _, _) ->
    (*Lists can be turned into arrays*)
    failwith "Unsupported"
  | Tuple (_, _) ->
    (*Tuples can be turned into records*)
    failwith "Unsupported"
  | Dictionary (label_opt, type_name) ->
    failwith "TODO -- link to dictionary provided by libNaaS" (*TODO*)
  | Empty -> failwith "Cannot translate empty type"
  | Tuple (label_opt, []) ->
    assert (label_opt = None);
    (*We cannot have values of type "void" in the target, we can only type
      functions with such a type.*)
    (Unit_Type, st)
  | UserDefinedType (label_opt, type_name) ->
    let type_name' = check_and_resolve_typename type_name in
    let (label_opt', st') = check_and_generate_name label_opt in
    let ty' = UserDefined_Type (label_opt', type_name')
    in (ty', st')
  | Boolean (label_opt, type_ann) ->
    if (type_ann <> []) then
      failwith "Boolean serialisation annotation not supported"; (*TODO*)
    let (label_opt', st') = check_and_generate_name label_opt
    in (Bool_Type label_opt', st')
  | Integer (label_opt, type_ann) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let metadata =
      List.fold_right (fun (name, ann) md ->
        match ann with
        | Ann_Int i ->
          if name = "byte_size" then
            let bits =
              match i with
              | 2 -> 16
              | 4 -> 32
              | 8 -> 64
              | _ -> failwith ("Unsupported integer precision: " ^
                               string_of_int i ^ " bytes")
            in { md with precision = bits }
          else failwith ("Unrecognised integer annotation: " ^ name)
        | Ann_Ident s ->
          if name = "signed" then
            let bool_value =
              match s with
              | "true" -> true
              | "false" -> false
              | _ -> failwith ("Unrecognised Boolean value: " ^ s)
            in { md with signed = bool_value }
          else failwith ("Unrecognised integer annotation: " ^ name)
        | _ -> failwith ("Unrecognised integer annotation: " ^ name))
        type_ann default_int_metadata
    in (Int_Type (label_opt', metadata), st')
  | IPv4Address label_opt ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let metadata = { signed = false; precision = 32 }
    in (Int_Type (label_opt', metadata), st')
  | String (label_opt, type_ann) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let vlen = Undefined (*FIXME determine from type_ann*) in
    let container_type =
      match vlen with
      | Undefined ->
        (*FIXME it's really important to specify stopping conditions, for the
          deserialiser to work -- plus this could also allow us to implement
          bounds checking. This also applies to "Dependent" below.*)
        Reference_Type (label_opt', Char_Type None)
      | Max _ -> Array_Type (label_opt', Char_Type None, vlen)
      | Dependent _ ->
        (*FIXME as in "Undefined" above, we need stopping conditions.*)
        Reference_Type (label_opt', Char_Type None)
    in (container_type, st')
  | Reference (label_opt, ty) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let (ty', st'') = naasty_of_flick_type st' ty
    in (Reference_Type (label_opt', ty'), st'')
  | RecordType (label_opt, tys, type_ann) ->
    if (type_ann <> []) then
      failwith "Record serialisation annotation not supported"; (*TODO*)
    let (type_identifier, st') = check_and_generate_typename label_opt in
    let (tys', st'') = fold_map ([], st') naasty_of_flick_type tys
    in (Record_Type (type_identifier, List.rev tys'), st'')


let rec naasty_of_flick_toplevel_decl (st : state) (tl : toplevel_decl) :
  (naasty_declaration * state) =
  match tl with
  | Type ty_decl ->
    let (ty', st') =
      update_empty_label ty_decl.type_name ty_decl.type_value
      |> naasty_of_flick_type st
    in (Type_Decl ty', st)
  | Function fn_decl ->
    (*FIXME!*)(Type_Decl (Bool_Type (Some (-1))), st)
  | Process (process_name, process_type, process_body) ->
    (*FIXME!*)(Type_Decl (Bool_Type (Some (-1))), st)
  | Include filename ->
    (*FIXME!*)(Type_Decl (Bool_Type (Some (-1))), st)

let naasty_of_flick_program (p : program) : (naasty_program * state) =
  fold_map ([], initial_state) naasty_of_flick_toplevel_decl p
