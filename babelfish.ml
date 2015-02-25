(*
   Translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

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

(*FIXME i'm ignoring annotations for the time being*)
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
    let (label_opt', st') = check_and_generate_name label_opt
    in (Bool_Type label_opt', st')
  | Integer (label_opt, type_ann) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let metadata = default_int_metadata(*FIXME this should depend on type_ann*)
    in (Int_Type (label_opt', metadata), st')
  | IPv4Address label_opt ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let metadata = { signed = false; precision = 32 }
    in (Int_Type (label_opt', metadata), st')
  | String (label_opt, type_ann) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let vlen = Undefined (*FIXME determine from type_ann*)
    in (Array_Type (label_opt', Char_Type None, vlen), st')
  | Reference (label_opt, ty) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let (ty', st'') = naasty_of_flick_type st' ty
    in (Reference_Type (label_opt', ty'), st'')
  | RecordType (label_opt, tys, type_ann(*FIXME unused*)) ->
    let (type_identifier, st') = check_and_generate_typename label_opt in
    let (tys', st'') = List.fold_right (fun ty (tys_acc, st_acc) ->
      let (ty', st_acc') = naasty_of_flick_type st_acc ty
      in (ty' :: tys_acc, st_acc')) tys ([], st')
    in (Record_Type (type_identifier, List.rev tys'), st'')
