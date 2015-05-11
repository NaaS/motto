(*
   Supporting definitions and functions for the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty
open State


let prog_indentation = 0
let no_indent = 0
let default_indentation = 2

(* NOTE in the basic pretty-printing functions below we don't terminate with
        semicolons, since these functions could be used compositionally.*)

let resolve_idx (scope : scope) (prefix : string) (st_opt : state option) (i : int) =
  match st_opt with
  | None -> prefix ^ string_of_int i
  | Some st ->
    begin
      match lookup_id scope st i with
      | None -> failwith ("Could not resolve idx " ^ string_of_int i ^ " in " ^
                          scope_to_str scope ^ " scope")
      | Some name -> name
    end
let no_prefix = ""
let ty_prefix = "ty_"
let ty_name = resolve_idx Type ty_prefix
let id_prefix = "id_"
let id_name = resolve_idx Term id_prefix

(*Extract identifier from a type*)
let idx_of_naasty_type = function
  | Int_Type (id_opt, _) -> id_opt
  | Bool_Type id_opt -> id_opt
  | Char_Type id_opt -> id_opt
  | Array_Type (id_opt, _, _) -> id_opt
  | Record_Type (ty_ident, _) -> Some ty_ident
  | Unit_Type -> failwith "Unit type cannot have idx"
  | UserDefined_Type (id_opt, _) -> id_opt
  | Pointer_Type (id_opt, _) -> id_opt
  | Size_Type id_opt -> id_opt
  | Static_Type (id_opt, _) -> id_opt
  | Fun_Type (id, _, _) -> Some id
  | Chan_Type (id_opt, _, _, _) -> id_opt

let update_empty_identifier (idx : identifier) (ty : naasty_type) =
  match ty with
  | Int_Type (id_opt, int_metadata) ->
    if id_opt = None then
      Int_Type (Some idx, int_metadata)
    else failwith "Cannot set an already-set index"
  | Bool_Type id_opt ->
    if id_opt = None then
      Bool_Type (Some idx)
    else failwith "Cannot set an already-set index"
  | Char_Type id_opt ->
    if id_opt = None then
      Char_Type (Some idx)
    else failwith "Cannot set an already-set index"
  | Array_Type (id_opt, naasty_type, array_size) ->
    if id_opt = None then
      Array_Type (Some idx, naasty_type, array_size)
    else failwith "Cannot set an already-set index"
  | Record_Type (ty_ident, fields) ->
    failwith "Cannot update index of this type."
  | Unit_Type -> ty
  | UserDefined_Type (id_opt, ty_ident) ->
    if id_opt = None then
      UserDefined_Type (Some idx, ty_ident)
    else failwith "Cannot set an already-set index"
  | Pointer_Type (id_opt, naasty_type) ->
    if id_opt = None then
      Pointer_Type (Some idx, naasty_type)
    else failwith "Cannot set an already-set index"
  | Size_Type id_opt ->
    if id_opt = None then
      Size_Type (Some idx)
    else failwith "Cannot set an already-set index"
  | Static_Type (id_opt, naasty_type) ->
    if id_opt = None then
      Static_Type (Some idx, naasty_type)
    else failwith "Cannot set an already-set index"
  | Fun_Type (_, _, _) ->
    failwith "Cannot update index of this type."
  | Chan_Type (id_opt, is_array, chan_direction, naasty_type) ->
    if id_opt = None then
      Chan_Type (Some idx, is_array, chan_direction, naasty_type)
    else failwith "Cannot set an already-set index"

(*Erase the identifier associated with a type. This is useful if we simply want
  to print out the type (without its associated identifier), such as if we're
  printing it as a parameter to "sizeof".
  NOTE we don't do anything exceptional if an identifier is not associated with
       a type.*)
let set_empty_identifier (ty : naasty_type) : naasty_type =
  match ty with
  | Int_Type (_, int_metadata) ->
    Int_Type (None, int_metadata)
  | Bool_Type _ ->
    Bool_Type None
  | Char_Type _ ->
    Char_Type None
  | Array_Type (_, naasty_type, array_size) ->
    (*FIXME recurse on naasty_type?*)
    Array_Type (None, naasty_type, array_size)
  | Record_Type (ty_ident, fields) ->
    failwith "Cannot update index of this type."
  | Unit_Type -> ty
  | UserDefined_Type (_, ty_ident) ->
    UserDefined_Type (None, ty_ident)
  | Pointer_Type (_, naasty_type) ->
    Pointer_Type (None, naasty_type)
  | Size_Type _ ->
    Size_Type None
  | Static_Type (_, naasty_type) ->
    Static_Type (None, naasty_type)
  | Fun_Type (_, _, _) ->
    failwith "Cannot update index of this type."
  | Chan_Type (_, is_array, chan_direction, naasty_type) ->
    Chan_Type (None, is_array, chan_direction, naasty_type)

let rec string_of_naasty_type ?st_opt:((st_opt : state option) = None) indent =
  function
  | Int_Type (id_opt, int_metadata) ->
    let prefix =
      if int_metadata.signed then "" else "u" in
    let suffix =
      (*This is checked during translation to make sure it's a sensible
        value: 16, 32, 64*)
      string_of_int int_metadata.precision in
    indn indent ^
    prefix ^ "int" ^ suffix ^ "_t" ^
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
  | Bool_Type id_opt ->
    indn indent ^
    "bool" ^
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
  | Char_Type id_opt ->
    indn indent ^
    "char" ^ (*FIXME signed vs unsigned?*)
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
    (*FIXME representation of string might lend itself better to C-style
      strings, to play nice with de/serialisers.*)
  | Array_Type (id_opt, naasty_type, array_size) ->
    let size = match array_size with
      | Undefined -> failwith "Arrays must have a defined size."
      | Max i -> string_of_int i
      | Dependent _ -> failwith "TODO"
    in indn indent ^
    (*FIXME notation might be wrong -- the brackets enclosing the size might
            need to appear to the right of the variable name.*)
    string_of_naasty_type ~st_opt no_indent naasty_type ^
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt ^ "[" ^ size ^ "]"
  (*Tuples will be encoded as records*)
  | Record_Type (ty_ident, fields) ->
    (*Record types won't appear nested -- instead, the nested record will be
      pulled up to a global scope as a separate record type.*)
    let body =
      List.map (fun s ->
        string_of_naasty_type ~st_opt (indent + default_indentation) s ^ ";")
       fields
      |> String.concat "\n"
    in indn indent ^ "typedef " ^
    "struct " ^
    "{\n" ^ body ^ "\n" ^ indn indent ^ "}" ^
    " " ^ ty_name st_opt ty_ident
  | Unit_Type -> indn indent ^ "void"
  | UserDefined_Type (id_opt, ty_ident) ->
    indn indent ^
    ty_name st_opt ty_ident ^
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
  | Pointer_Type (id_opt, naasty_type) ->
    string_of_naasty_type ~st_opt indent naasty_type ^ " *" ^
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
  | Size_Type id_opt ->
    indn indent ^
    "size_t" ^
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
  | Static_Type (id_opt, naasty_type) ->
    indn indent ^ "static " ^
    string_of_naasty_type ~st_opt no_indent naasty_type ^
    bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
  | Fun_Type (id, res_ty, arg_tys) ->
    string_of_naasty_type ~st_opt indent res_ty ^
    " " ^ id_name st_opt id ^ " " ^
    "(" ^
    String.concat ", "
      (List.map (string_of_naasty_type ~st_opt no_indent) arg_tys) ^
    ")"
  | Chan_Type (id_opt, is_array, chan_direction, naasty_type) ->
    (*NOTE chan_direction and naasty_type are metadata, as far as
           pretty-printing are concerned, that aren't displayed here.*)
    indn indent ^
    begin
      match is_array with
      | true ->
        "std::vector<TaskBuffer *> &" ^
        bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
      | false ->
        "TaskBuffer &"(*FIXME check syntax -- i'm not sure we have singleton
                         channels in the C++ implementation examples*) ^
        bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
    end

let rec string_of_naasty_expression ?st_opt:((st_opt : state option) = None) = function
  | Int_Value i -> string_of_int i
  | Plus (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") + (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Var id -> id_name st_opt id
  | Call_Function (id, es) ->
    let arg_s =
      List.map (string_of_naasty_expression ~st_opt) es
      |> String.concat ", " in
    id_name st_opt id ^ " (" ^ arg_s ^ ")"
  | GEq (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") >= (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Gt (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") > (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Cast (ty, e) ->
    "(" ^ string_of_naasty_type no_indent ~st_opt ty ^ ")" ^
    "(" ^ string_of_naasty_expression ~st_opt e ^ ")"
  | RecordProjection (Dereference record_ref, field) ->
    (*NOTE this syntactic sugaring is handled directly*)
    "(" ^ string_of_naasty_expression ~st_opt record_ref ^ ")" ^
    "->" ^
    "(" ^ string_of_naasty_expression ~st_opt field ^ ")"
  | Dereference e ->
    "*" ^ "(" ^ string_of_naasty_expression ~st_opt e ^ ")"
  | RecordProjection (record, field) ->
    "(" ^ string_of_naasty_expression ~st_opt record ^ ")" ^
    "." ^
    "(" ^ string_of_naasty_expression ~st_opt field ^ ")"
  | Address_of e ->
    "&(" ^ string_of_naasty_expression ~st_opt e ^ ")"
  | And (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") && (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Or (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") || (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Not e ->
    "!(" ^ string_of_naasty_expression ~st_opt e ^ ")"
  | Equals (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") = (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Lt (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") < (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Minus (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") - (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Times (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") * (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | LEq (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") <= (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"

let rec string_of_naasty_statement ?st_opt:((st_opt : state option) = None)
          ?print_semicolon:(print_semicolon : bool = true) indent statement =
  let terminal =
    if print_semicolon then ";" else "" in
  match statement with
  | Declaration (ty, e_opt) ->
    (*NOTE assuming that types can only be defined globally,
           but they can be used in local variable declarations.*)
    let definition =
      match e_opt with
      | None -> ""
      | Some e -> " = " ^ string_of_naasty_expression ~st_opt e
    in string_of_naasty_type ~st_opt indent ty ^ definition ^ terminal
  | Seq (stmt1, stmt2) ->
    string_of_naasty_statement ~st_opt indent stmt1 ^ "\n" ^
    string_of_naasty_statement ~st_opt indent stmt2
  | Assign (lvalue, e) ->
    indn indent ^ string_of_naasty_expression ~st_opt lvalue ^ " = " ^
    string_of_naasty_expression ~st_opt e ^ terminal
  | Increment (id, e) ->
    indn indent ^ id_name st_opt id ^ " += " ^
    string_of_naasty_expression ~st_opt e ^ terminal
  | For ((id, condition, increment), body) ->
    (*FIXME check if the target syntax is correct*)
    indn indent ^ "for (" ^
    string_of_naasty_type ~st_opt no_indent id ^ "; " ^
    string_of_naasty_expression ~st_opt condition ^ "; " ^
    string_of_naasty_statement ~st_opt no_indent ~print_semicolon:false increment ^ ") {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) body ^
    "\n" ^ indn indent ^ "}"
  | If (e, stmt1, stmt2) ->
    indn indent ^ "if (" ^ string_of_naasty_expression ~st_opt e ^ ") {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) stmt1 ^
    "\n" ^
    indn indent ^ "} else {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) stmt2 ^
    "\n" ^
    indn indent ^ "}"
  | If1 (e, stmt1) ->
    indn indent ^ "if (" ^ string_of_naasty_expression ~st_opt e ^ ") {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) stmt1 ^
    "\n" ^
    indn indent ^ "}"
  | Break -> indn indent ^ "break" ^ terminal
  | Continue -> indn indent ^ "continue" ^ terminal
(*
  | WriteToChan of identifier * identifier
  | ReadFromChan of identifier * identifier
*)
  | Return e_opt ->
    let f e = " " ^ "(" ^ string_of_naasty_expression ~st_opt e ^ ")" in
    indn indent ^ "return" ^  bind_opt f "" e_opt ^ terminal
  | Skip -> indn indent ^ "/*skip*/"
  | Commented (Skip, comment) ->
    (*Simply print the comment*)
    indn indent ^ "// " ^ comment
  | Commented (stmt, comment) ->
    (*First print the statement, then the comment*)
    string_of_naasty_statement ~st_opt indent stmt ^ " // " ^ comment
  | St_of_E e ->
    indn indent ^ string_of_naasty_expression ~st_opt e ^ terminal

let string_of_naasty_function ?st_opt:((st_opt : state option) = None) indent naasty_function =
  let arg_types_s =
   List.map (string_of_naasty_type ~st_opt indent) naasty_function.arg_tys
   |> String.concat ", " in
string_of_naasty_type ~st_opt indent naasty_function.ret_ty ^ " " ^
  id_name st_opt naasty_function.id ^ " " ^
    "(" ^ arg_types_s ^ ") {\n" ^
    string_of_naasty_statement ~st_opt (indent + default_indentation)
      naasty_function.body ^ "\n" ^
    "}"

let string_of_naasty_declaration ?st_opt:((st_opt : state option) = None) indent = function
  | Type_Decl naasty_type -> string_of_naasty_type ~st_opt indent naasty_type
  | Fun_Decl naasty_function -> string_of_naasty_function ~st_opt indent naasty_function
  | Stmt naasty_statement -> string_of_naasty_statement ~st_opt indent naasty_statement

let string_of_naasty_program ?st_opt:((st_opt : state option) = None) indent prog =
  prog
  |> List.map
       (string_of_naasty_declaration ~st_opt indent)
  |> String.concat "\n"

(*Extends a scope by adding a mapping between a name and an index.
  NOTE we don't check for clashes! thus the _unsafe prefix*)
let extend_scope_unsafe (scope : scope) (st : state) ?ty_opt:(ty_opt = None) (id : string) : Naasty.identifier * state =
  let ty_opt' =
    (*If we're given a type, but it isn't associated with a variable index, then
      update the type to associate it with the index we have.*)
    if ty_opt <> None && idx_of_naasty_type (the ty_opt) = None then
      Some (update_empty_identifier st.next_symbol (the ty_opt))
    else ty_opt in
  match scope with
  | Type ->
    (st.next_symbol,
     { st with
       type_symbols = (id, st.next_symbol, ty_opt') :: st.type_symbols;
       next_symbol = 1 + st.next_symbol;
     })
  | Term ->
    (st.next_symbol,
     { st with
       term_symbols = (id, st.next_symbol, ty_opt') :: st.term_symbols;
       next_symbol = 1 + st.next_symbol;
     })

(*Adds a fresh identifier to the scope, based on a specific prefix, to which
  we concatenate a numeric suffix/index*)
let mk_fresh (scope : scope) ?ty_opt:(ty_opt = None) (id : string) (min_idx : int) (st : state) :
  string * Naasty.identifier * state =
  if min_idx < 0 then
    failwith "min_idx must be non-negative"
  else
    let idx = ref min_idx in
    while (lookup_name scope st (id ^ string_of_int !idx) <> None) do
      idx := 1 + !idx
    done;
    let name = id ^ string_of_int !idx in
    let (idx, st') = extend_scope_unsafe scope st ~ty_opt name
    in (name, idx, st')

(*Indicates if a name is fresh in either scope*)
let is_fresh (id : string) (st : state) : bool =
  lookup_name Term st id = None && lookup_name Type st id = None

(*
  Applies a transformation 'f' to the state-index of a symbol that's in turn
   indexed in 'names' by a placeholder 'id' -- if 'id' turns out to be a
   placeholder.

  Parameters:
   'scheme' is the phrase (type, expression, etc) we are evaluating for whether
      a substitution should take place. We have no information about this
      scheme, since at this level we don't need it. Info about the scheme is
      encapsulated in 'f'.
   'id' is the identifier we are evaluating for this substitution -- if the
      substitution goes ahead, then we'll be substitution some value for this
      identifier. This value will have the same type as 'scheme'. We don't need
      to know this type, or how the substitution itself will be done -- that is
      encapsulated in 'f'.
   'f' carries out the substitution, if we determine that a substitution should
      take place.
   'names' is a list of names we'll consult to determine what name a placeholder
      should get. If selected, a name will be added to a scope (unless it
      already exists -- unless 'fresh' isn't set to true).

   'type_mode' determines whether the mapped-to name is of Type or Term scope.

   'st' state.
   'fresh' asserts that each name in 'names' is fresh wrt 'st'
*)
let substitute (fresh : bool) (names : string list) (type_mode : bool)
      (scheme : 'a) (st : state) (id : identifier)  (f : identifier -> 'a) : 'a * state =
  if id > 0 then
    (*Identifier is not a placeholder, so return the scheme unchanged.*)
    (scheme, st)
  else if id = 0 then
    failwith "Template placeholder cannot be 0 -- this value is undefined."
  else
    (*The placeholder's value is used to perform a lookup on the list of names
      provided. The placeholder will be "mapped" to that name -- to be precise,
      it's mapped to the index (in the state, NOT in the list of names) of
      that name. If the name doesn't have an index then we create one for it,
      and update the state.*)
    let local_name = List.nth names (abs id - 1) in
    let id', st' =
      if not fresh then
        (*Look it up from the state*)
        let scope = if type_mode then Type else Term in
        match lookup_name scope st local_name with
        | None ->
            failwith ("Undeclared " ^ scope_to_str scope ^ ": " ^ local_name)
        | Some i -> (i, st)
      else
        (*Generate a fresh name and update the state*)
        if type_mode then
          match lookup_name Type st local_name with
          | None ->
            extend_scope_unsafe Type st local_name
          | Some idx ->
            if forbid_shadowing then
              failwith ("Already declared type: " ^ local_name)
            else
              (idx, st)
        else
          match lookup_name Term st local_name with
          | None ->
            extend_scope_unsafe Term st local_name
          | Some idx ->
            if forbid_shadowing then
              failwith ("Already declared identifier: " ^ local_name)
            else
              (idx, st)
    in (f id', st')

(*Optionally applies the 'substitute' function, depending on whether an
  identifier is provided. Remember that the purpose of 'substitute' is to map a
  placeholder (which is presented as a form of identifier -- in practice a
  negative integer) with some other value (type or expression or whatever).

  For the meaning of the parameters, see the definition of 'substitute' above.*)
let substitute_opt (fresh : bool) (names : string list) (type_mode : bool)
      (scheme : 'a) (st : state) (id_opt : identifier option)
      (f : identifier -> 'a) : 'a * state =
  match id_opt with
  | None -> (scheme, st)
  | Some id ->
    substitute fresh names type_mode scheme st id f

(*Instantiates a naasty_type scheme with a set of names*)
let rec instantiate_type (fresh : bool) (names : string list) (st : state)
      (scheme : naasty_type) : naasty_type * state =
  match scheme with
  | Int_Type (id_opt, int_metadata) ->
    substitute_opt fresh names false scheme st id_opt (fun id' ->
      Int_Type (Some id', int_metadata))
  | Bool_Type id_opt ->
    substitute_opt fresh names false scheme st id_opt (fun id' ->
      Bool_Type (Some id'))
  | Char_Type id_opt ->
    substitute_opt fresh names false scheme st id_opt (fun id' ->
      Char_Type (Some id'))
  | Array_Type (id_opt, naasty_type, array_size) ->
    let naasty_type', st' =
      instantiate_type fresh names st naasty_type in
    if naasty_type' = naasty_type then
      begin
        assert (st = st');
        substitute_opt fresh names false scheme st id_opt (fun id' ->
        Array_Type (Some id', naasty_type, array_size))
      end
    else
      Array_Type (id_opt, naasty_type', array_size)
      |> instantiate_type fresh names st'
  | Record_Type (ty_ident, fields) ->
    let ty_ident', st' =
      substitute fresh names true ty_ident st ty_ident (fun x -> x) in
    let fields', st'' =
      fold_map ([], st') (instantiate_type fresh names) fields in
    (Record_Type (ty_ident', fields'), st'')
  | Unit_Type -> (Unit_Type, st)
  | UserDefined_Type (id_opt, ty_ident) ->
    let ty_ident', st' =
      substitute fresh names true ty_ident st ty_ident (fun x -> x) in
    let scheme' = UserDefined_Type (id_opt, ty_ident') in
    substitute_opt fresh names false scheme' st' id_opt (fun id' ->
      UserDefined_Type (Some id', ty_ident'))
  | Pointer_Type (id_opt, naasty_type) ->
    let naasty_type', st' =
      instantiate_type fresh names st naasty_type in
    if naasty_type' = naasty_type then
      begin
        assert (st = st');
        substitute_opt fresh names false scheme st id_opt (fun id' ->
        Pointer_Type (Some id', naasty_type))
      end
    else
      Pointer_Type (id_opt, naasty_type')
      |> instantiate_type fresh names st'
  | Size_Type id_opt ->
    substitute_opt fresh names false scheme st id_opt (fun id' ->
      Size_Type (Some id'))
  | Static_Type (id_opt, naasty_type) ->
    let naasty_type', st' =
      instantiate_type fresh names st naasty_type in
    if naasty_type' = naasty_type then
      begin
        assert (st = st');
        substitute_opt fresh names false scheme st id_opt (fun id' ->
        Static_Type (Some id', naasty_type))
      end
    else
      Static_Type (id_opt, naasty_type')
      |> instantiate_type fresh names st'
  | Fun_Type (id, res_ty, arg_tys) ->
    let id', st' =
      substitute fresh names false id st id (fun x -> x) in
    let res_ty', st'' =
      instantiate_type fresh names st' res_ty in
    let arg_tys', st''' =
      fold_map ([], st'') (instantiate_type fresh names) arg_tys in
    (Fun_Type (id', res_ty', arg_tys'), st''')
  | Chan_Type (id_opt, is_array, chan_direction, naasty_type) ->
    let naasty_type', st' =
      instantiate_type fresh names st naasty_type in
    if naasty_type' = naasty_type then
      begin
        assert (st = st');
        substitute_opt fresh names false scheme st id_opt (fun id' ->
          Chan_Type (Some id', is_array, chan_direction, naasty_type))
      end
    else
      Chan_Type (id_opt, is_array, chan_direction, naasty_type')
      |> instantiate_type fresh names st'

(*Instantiates a naasty_statement scheme with a set of names*)
let rec instantiate_expression (fresh : bool) (names : string list) (st : state)
      (scheme : naasty_expression) : naasty_expression * state =
  let unary_op_inst e f =
    let (e', st') = instantiate_expression fresh names st e
    in (f e', st') in
  let binary_op_inst e1 e2 f =
    let (e1', st') = instantiate_expression fresh names st e1 in
    let (e2', st'') = instantiate_expression fresh names st' e2
    in (f e1' e2', st'')
  in match scheme with
  | Var id ->
    let id', st' =
      substitute fresh names false id st id (fun x -> x)
    in (Var id', st')
  | Int_Value _
  | Bool_Value _ -> (scheme, st)
  | Not e -> unary_op_inst e (fun e' -> Not e')
  | Abs e -> unary_op_inst e (fun e' -> Abs e')
  | And (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> And (e1', e2'))
  | Or (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Or (e1', e2'))
  | Plus (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Plus (e1', e2'))
  | Equals (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Equals (e1', e2'))
  | Lt (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Lt (e1', e2'))
  | Minus (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Minus (e1', e2'))
  | Times (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Times (e1', e2'))
  | Mod (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Mod (e1', e2'))
  | Quotient (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Quotient (e1', e2'))
  | Call_Function (id, es) ->
    let id', st' =
      substitute fresh names false id st id (fun x -> x) in
    let es', st'' =
      fold_map ([], st') (instantiate_expression fresh names) es
    in (Call_Function (id', es'), st'')
  | GEq (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> GEq (e1', e2'))
  | Gt (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Gt (e1', e2'))
  | Cast (ty, e) ->
    let ty', st' = instantiate_type fresh names st ty in
    let e', st'' =
      instantiate_expression fresh names st' e
    in (Cast (ty', e'), st'')
  | Dereference e -> unary_op_inst e (fun e' -> Dereference e')
  | RecordProjection (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> RecordProjection (e1', e2'))
  | Address_of e -> unary_op_inst e (fun e' -> Address_of e')

(*Instantiates a naasty_statement scheme with a set of names*)
let rec instantiate_statement (fresh : bool) (names : string list) (st : state)
      (scheme : naasty_statement) : naasty_statement * state =
  match scheme with
  | Declaration (ty, e_opt) ->
    let (ty', st') = instantiate_type fresh names st ty in
    let (e_opt', st'') =
      match e_opt with
      | None -> (e_opt, st')
      | Some e ->
        let (e', st'') = instantiate_expression fresh names st' e
        in (Some e', st'')
    in (Declaration (ty', e_opt'), st'')
  | Seq (stmt1, stmt2) ->
    let (stmt1', st') = instantiate_statement fresh names st stmt1 in
    let (stmt2', st'') = instantiate_statement fresh names st' stmt2
    in (Seq (stmt1', stmt2'), st'')
  | Assign (lvalue, e) ->
    let (lvalue', st') = instantiate_expression fresh names st lvalue in
    let (e', st'') = instantiate_expression fresh names st' e
    in (Assign (lvalue', e'), st'')
  | Return e_opt ->
    let (e_opt', st') =
      match e_opt with
      | None -> (e_opt, st)
      | Some e ->
        let (e', st') = instantiate_expression fresh names st e
        in (Some e', st')
    in (Return e_opt', st')
  | Skip -> (Skip, st)
  | If (e, stmt1, stmt2) ->
    let (e', st') = instantiate_expression fresh names st e in
    let (stmt1', st'') = instantiate_statement fresh names st' stmt1 in
    let (stmt2', st''') = instantiate_statement fresh names st'' stmt2
    in (If (e', stmt1', stmt2'), st''')
  | If1 (e, stmt1) ->
    let (e', st') = instantiate_expression fresh names st e in
    let (stmt1', st'') = instantiate_statement fresh names st' stmt1
    in (If1 (e', stmt1'), st'')
  | Increment (id, e) ->
    let id', st' =
      substitute fresh names false id st id (fun x -> x) in
    let (e', st'') = instantiate_expression fresh names st' e
    in (Increment (id', e'), st'')
  | Commented (Skip, _) -> (scheme, st)
  | Commented (stmt, comment) ->
    let (stmt', st') = instantiate_statement fresh names st stmt
    in (Commented (stmt', comment), st')
  | St_of_E e ->
    let (e', st') = instantiate_expression fresh names st e
    in (St_of_E e', st')
  | Break -> (Break, st)
  | Continue -> (Continue, st)
  | WriteToChan (chan_id, var_id) ->
    let chan_id', st' =
      substitute fresh names false chan_id st chan_id (fun x -> x) in
    let var_id', st'' =
      substitute fresh names false var_id st' var_id (fun x -> x)
    in (WriteToChan (chan_id', var_id'), st'')
  | ReadFromChan (chan_id, var_id) ->
    let chan_id', st' =
      substitute fresh names false chan_id st chan_id (fun x -> x) in
    let var_id', st'' =
      substitute fresh names false var_id st' var_id (fun x -> x)
    in (ReadFromChan (chan_id', var_id'), st'')
  | For ((id, condition, increment), body) ->
    let id', st' = instantiate_type fresh names st id in
    let (condition', st'') = instantiate_expression fresh names st' condition in
    let (increment', st''') = instantiate_statement fresh names st'' increment in
    let (body', st4) = instantiate_statement fresh names st''' body
    in (For ((id', condition', increment'), body'), st4)

(*Instantiates a naasty_function scheme with a set of names*)
let rec instantiate_function (fresh : bool) (names : string list) (st : state)
      (scheme : naasty_function) : naasty_function * state =
  let id', st' =
    substitute fresh names false scheme.id st scheme.id (fun x -> x) in
  let (arg_tys', st'') =
    fold_map ([], st') (instantiate_type fresh names) scheme.arg_tys in
  let (ret_ty', st''') = instantiate_type fresh names st'' scheme.ret_ty in
  let (stmt', st4) = instantiate_statement fresh names st''' scheme.body
  in ({id = id'; arg_tys = arg_tys'; ret_ty = ret_ty'; body = stmt'}, st4)

(*Takes a record type specification and adds fields to the end, in order.
  This is used to extend a type specification to fit the data model.*)
let add_fields_to_record (decl : naasty_declaration)
      (additional_tys : naasty_type list) : naasty_declaration =
  match decl with
  | Type_Decl (Record_Type (ty_id, tys)) ->
    Type_Decl (Record_Type (ty_id, tys @ additional_tys))
  | _ -> failwith "Tried to add fields to non-record."

(*Assigns to a collection of variables the value of an expression*)
let lift_assign (recipients : identifier list) (definiens : naasty_expression) :
  naasty_statement list =
  List.map (fun recipient -> Assign (Var recipient, definiens)) recipients

(*Sequentially composed two statements but eliminate any Skip steps*)
let mk_seq (s1 : naasty_statement) (s2 : naasty_statement) : naasty_statement =
  match s1, s2 with
  | Skip, Skip -> Skip
  | Skip, _ -> s2
  | _, Skip -> s1
  | _, _ -> Seq (s1, s2)

(*Concats a list of statements into the smallest equivalent sequence of statements*)
let rec concat (sts : naasty_statement list) : naasty_statement =
  match sts with
  | [] -> Skip
  | [s] -> s
  | [s1; s2] -> mk_seq s1 s2
  | s1 :: s2 :: rest ->
    concat rest
    |> mk_seq s2
    |> mk_seq s1

(* Turn a list of identifiers into a projection from nested records.
   FIXME here we assume throughout that we need dereference*)
let rec nested_fields (field_idents : identifier list) : naasty_expression =
  match field_idents with
  | [field; record] ->
    RecordProjection (Dereference (Var record), Var field)
  | field :: rest ->
    let record = nested_fields rest in
    RecordProjection (Dereference record, Var field)
  | _ ->
    failwith "There needs to be at least one record and one field: field_idents needs to contain at least two items."

(*
  Table of identifiers, the number of times they are assigned to in this
  scope and child scopes, and, if the value of that field is 1, then we could
   also have the value they could be replaced with*)
type inliner_table_entry =
  {
    id : identifier;
    parameter : bool;
    update_count : int;
    ref_count : int;
    initialisation : naasty_expression option;
    assignment : naasty_expression option;
  }

let rec count_var_references_in_naasty_expr (st : state)
  (expr : naasty_expression) (table : inliner_table_entry list) : inliner_table_entry list =
  match expr with
  | Var idx ->
    let no_idx_entries =
      List.fold_right (fun (entry : inliner_table_entry) acc ->
        if entry.id = idx then acc + 1 else acc) table 0 in
    assert (no_idx_entries >= 0);
    if no_idx_entries = 1 then
      (*Increment times we've referenced this idx*)
      List.map (fun (entry : inliner_table_entry) ->
        if entry.id = idx then
          { entry with ref_count = entry.ref_count + 1 }
        else entry) table
    else
      failwith ("Undeclared variable: " ^ string_of_int no_idx_entries ^
                " records for the same idx " ^
                string_of_int idx ^ " -- variable " ^
                resolve_idx Term no_prefix (Some st) idx)
  | Int_Value _
  | Bool_Value _ -> table
  | Not e
  | Abs e
  | Dereference e
  | Address_of e
  | Cast (_, e) ->
    count_var_references_in_naasty_expr st e table
  | And (e1, e2)
  | Or (e1, e2)
  | Plus (e1, e2)
  | Equals (e1, e2)
  | Lt (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | GEq (e1, e2)
  | Gt (e1, e2)
  | RecordProjection (e1, e2)
  | LEq (e1, e2) ->
    count_var_references_in_naasty_expr st e1 table
    |> count_var_references_in_naasty_expr st e2
  | Call_Function (_ (*ignore function identifier*), exprs) ->
    List.fold_right (fun expr acc ->
      count_var_references_in_naasty_expr st expr acc) exprs table

let rec count_var_references_in_naasty_stmt (st : state)
  (stmt : naasty_statement) (table : inliner_table_entry list) : inliner_table_entry list =
  match stmt with
  | Declaration (_, expr_opt) ->
    begin
    match expr_opt with
    | None -> table
    | Some expr -> count_var_references_in_naasty_expr st expr table
    end
  | Seq (stmt1, stmt2) ->
    count_var_references_in_naasty_stmt st stmt1 table
    |> count_var_references_in_naasty_stmt st stmt2
  | Assign (_ (*ignore lvalue*), expr)
  | Increment (_, expr)
  | St_of_E expr ->
    count_var_references_in_naasty_expr st expr table
  | For ((ty, expr, stmt), body) ->
    count_var_references_in_naasty_expr st expr table
    |> count_var_references_in_naasty_stmt st stmt
    |> count_var_references_in_naasty_stmt st body
  | Return expr_opt ->
    begin
      match expr_opt with
      | None -> table
      | Some expr -> count_var_references_in_naasty_expr st expr table
    end
  | Break
  | Continue
  | Skip -> table
  | Commented (stmt, _) -> count_var_references_in_naasty_stmt st stmt table

let inliner_table_entry_to_string ?st_opt:((st_opt : state option) = None)
      (entry : inliner_table_entry) =
  "id=" ^ string_of_int entry.id ^
  bind_opt (fun st ->
    " (" ^ resolve_idx Term no_prefix st_opt entry.id ^ ")") "" st_opt ^ "; " ^
  "parameter=" ^ string_of_bool entry.parameter ^ "; " ^
  "update_count=" ^ string_of_int entry.update_count ^ "; " ^
  "ref_count=" ^ string_of_int entry.ref_count ^ "; " ^
  "initialisation=" ^ bind_opt (string_of_naasty_expression ~st_opt) "<None>"
    entry.initialisation ^ "; " ^
  "assignment=" ^ bind_opt (string_of_naasty_expression ~st_opt) "<None>"
    entry.assignment

(*Removes redundant definitions that are introduced in a profligate manner by
  naasty_of_flick_expr*)
(*For analysis just mutate the inliner_table_entry*)
let rec inliner_analysis (st : state) (stmt : naasty_statement)
  (ctxt_acc : identifier list)
  (table : inliner_table_entry list) : inliner_table_entry list =
  match stmt with
  | Declaration (ty, expr_opt) ->
    begin
    match idx_of_naasty_type ty with
    | None ->
      failwith "Declaration must contain an identifier name, not just mention a type!"
    | Some idx ->
      let no_idx_entries =
        List.fold_right (fun (entry : inliner_table_entry) acc ->
          if entry.id = idx then acc + 1 else acc) table 0 in
      assert (no_idx_entries >= 0);
      if no_idx_entries = 0 then
        {
          id = idx;
          parameter = false;
          update_count = if expr_opt = None then 0 else 1;
          ref_count = 0;
          initialisation = expr_opt;
          assignment = None;
        } :: table
      else if no_idx_entries = 1 then
        (*Increment times we've assigned to this idx*)
        List.map (fun (entry : inliner_table_entry) ->
          if entry.id = idx then
            if entry.initialisation <> None then
              failwith "Variable has already been initialised"
            else
              { entry with update_count = entry.update_count + 1;
                           initialisation = expr_opt }
          else entry) table
      else
        failwith "Impossible: multiple records for the same idx"
    end

  | Assign (Var idx, expr) ->
    let no_idx_entries =
      List.fold_right (fun (entry : inliner_table_entry) acc ->
        if entry.id = idx then acc + 1 else acc) table 0 in
    assert (no_idx_entries >= 0);
    if no_idx_entries = 1 then
      (*Increment times we've assigned to this idx*)
      List.map (fun (entry : inliner_table_entry) ->
        if entry.id = idx then
          if entry.initialisation <> None then
            failwith "Variable has already been initialised"
          else
            { entry with update_count = entry.update_count + 1;
                         assignment = Some expr }
        else entry) table
    else
      failwith ("Impossible: " ^ string_of_int no_idx_entries ^
                " records for the same idx " ^
                string_of_int idx ^ " -- variable " ^
                resolve_idx Term no_prefix (Some st) idx)

  | Increment (idx, expr) ->
    let no_idx_entries =
      List.fold_right (fun (entry : inliner_table_entry) acc ->
        if entry.id = idx then acc + 1 else acc) table 0 in
    assert (no_idx_entries >= 0);
    (*We MUST have at least initialised this variable*)
    if no_idx_entries = 0 then
      failwith "Uninitialised variable"
    else if no_idx_entries = 1 then
      (*Increment times we've assigned to this idx*)
      List.map (fun (entry : inliner_table_entry) ->
        if entry.id = idx then
          if entry.initialisation <> None then
            failwith "Variable has already been initialised"
          else
            { entry with update_count = entry.update_count + 1;
                         assignment = Some (Plus (Var idx, expr)) }
        else entry) table
    else
      failwith ("Impossible: multiple records for the same idx " ^
                string_of_int idx ^ " -- variable " ^
                resolve_idx Term no_prefix (Some st) idx)

  | Commented (stmt', _) ->
    inliner_analysis st stmt' ctxt_acc table

  | For ((cursor, test, update), body) ->
    inliner_analysis st body ctxt_acc table

  | Seq (stmt1, stmt2) ->
    inliner_analysis st stmt1 ctxt_acc table
    |> inliner_analysis st stmt2 ctxt_acc
  | _ -> table

(*
   Remove entries where update_count <> 1 and ref_count <> 1,
   find those where assignment is some Var, and inline those into other
   expressions in the table. add these to the deletion line

   compute substitutions for variables
   and deletions of assignments and declarations
*)
let variables_to_be_inlined : inliner_table_entry list -> inliner_table_entry list =
  List.filter (fun entry ->
    entry.parameter = false &&
    entry.update_count = 1 &&
    entry.ref_count = 1)

let rec naasty_expression_weight = function
  | Int_Value _
  | Bool_Value _ -> 0
  | Var _ -> 1
  | Abs e
  | Not e
  | Dereference e
  | Cast (_, e)
  | Address_of e -> naasty_expression_weight e + 1
  | And (e1, e2)
  | Or (e1, e2)
  | Plus (e1, e2)
  | Equals (e1, e2)
  | Lt (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | GEq (e1, e2)
  | Gt (e1, e2)
  | RecordProjection (e1, e2)
  | LEq (e1, e2) ->
    naasty_expression_weight e1 +
    naasty_expression_weight e2 + 1
  | Call_Function (_, exprs) ->
    List.fold_right (fun expr acc -> acc + naasty_expression_weight expr) exprs 1

let inliner_table_entry_weight entry =
  bind_opt naasty_expression_weight 0 entry.initialisation +
  bind_opt naasty_expression_weight 0 entry.assignment

let inliner_table_entry_order entry1 entry2 =
  let entry1_weight = inliner_table_entry_weight entry1 in
  let entry2_weight = inliner_table_entry_weight entry2 in
  if entry1_weight = entry2_weight then 0
  else if entry1_weight > entry2_weight then 1
  else
    begin
      assert (entry1_weight < entry2_weight);
      -1
    end

type substitution = (identifier * naasty_expression) list

(*Generate substitution from inliner_table_entry*)
let inliner_to_subst (table : inliner_table_entry list) : substitution =
  let rec inliner_to_subst' (table : inliner_table_entry list) (acc : substitution) : substitution =
  match table with
  | [] -> acc
  | (entry :: rest) ->
    (*NOTE here could assert that update_count and ref_count are both =1*)
    if List.mem_assoc entry.id acc then
      failwith "Substitution variable appears more than once in the substitution"
    else if entry.assignment = None then
      failwith "Nothing to substitute"
    else
      inliner_to_subst' rest ((entry.id, the entry.assignment) :: acc)
  in inliner_to_subst' table []

module Identifier_Set = Set.Make(
 struct
   type t = identifier
     (*FIXME unsure if i should use "compare" from Pervasives, since using
       polymorphic compare seems bad, unless the compiler inlines a better
       definition.*)
   let compare x y =
     if x = y then 0
     else if x < y then -1
     else 1
 end)

let rec free_vars (expr : naasty_expression) (acc : Identifier_Set.t) : Identifier_Set.t =
  match expr with
  | Var id -> Identifier_Set.add id acc

  | Int_Value _
  | Bool_Value _ -> acc

  | Cast
      (_ (*we don't couny any identifiers used in types*),
       e) ->
    free_vars e acc

  | Call_Function
      (_ (*we don't count the function name as a free variable*),
       exprs) ->
    List.fold_right free_vars exprs acc

  | Not e
  | Address_of e
  | Dereference e
  | Abs e -> free_vars e acc

  | And (e1, e2)
  | Or (e1, e2)
  | Plus (e1, e2)
  | Equals (e1, e2)
  | Lt (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | GEq (e1, e2)
  | Gt (e1, e2)
  | RecordProjection (e1, e2)
  | LEq (e1, e2) ->
    List.fold_right free_vars [e1; e2] acc

let rec subst_expr (subst : substitution) (expr : naasty_expression) : naasty_expression =
  let unary_op_inst e f = f (subst_expr subst e) in
  let binary_op_inst e1 e2 f =
    f (subst_expr subst e1) (subst_expr subst e2)
  in match expr with
  | Var id ->
    if List.mem_assoc id subst then
      List.assoc id subst
    else expr
  | Int_Value _
  | Bool_Value _ -> expr
  | Cast (ty, e) -> unary_op_inst e (fun e' -> Cast (ty, e'))
  | Not e -> unary_op_inst e (fun e' -> Not e')
  | Abs e -> unary_op_inst e (fun e' -> Abs e')
  | And (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> And (e1', e2'))
  | Or (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Or (e1', e2'))
  | Plus (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Plus (e1', e2'))
  | Equals (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Equals (e1', e2'))
  | Lt (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Lt (e1', e2'))
  | Minus (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Minus (e1', e2'))
  | Times (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Times (e1', e2'))
  | Mod (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Mod (e1', e2'))
  | Quotient (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Quotient (e1', e2'))
  | Call_Function (id, es) ->
    let es' = List.map (subst_expr subst) es in
    Call_Function (id, es')
  | GEq (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> GEq (e1', e2'))
  | Gt (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Gt (e1', e2'))
  | Dereference e -> unary_op_inst e (fun e' -> Dereference e')
  | RecordProjection (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> RecordProjection (e1', e2'))
  | Address_of e -> unary_op_inst e (fun e' -> Address_of e')
  | LEq (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> LEq (e1', e2'))

let rec subst_stmt ?subst_assignee:((subst_assignee : bool) = false)
          (subst : substitution) (stmt : naasty_statement) : naasty_statement =
  match stmt with
  | Break
  | Continue -> stmt
  | St_of_E e -> St_of_E (subst_expr subst e)
  | Increment (id, e) ->
    Increment (id (*we don't substitute into the id, since we cannot map it into
                  an expression -- unlike Assign statements.*),
               subst_expr subst e)
  | Commented (stmt', str) ->
    Commented (subst_stmt subst stmt', str)
  | Declaration (ty, e_opt) ->
    Declaration (ty (*we don't substitute into the type, we don't expect to find
                      expressions there*),
                 bind_opt (fun e -> Some (subst_expr subst e)) None e_opt)
  | Seq (stmt1, stmt2) ->
    Seq (subst_stmt subst stmt1, subst_stmt subst stmt2)
  | Assign (e1, e2) ->
    let e1' =
      if subst_assignee then
        subst_expr subst e1
      else e1 in
    Assign (e1', subst_expr subst e2)
  | For ((ty, cond, inc), body) ->
    let cond' = subst_expr subst cond in
    let inc' = subst_stmt subst inc in
    let body' = subst_stmt subst body in
    For ((ty, cond', inc'), body')
  | If (cond, stmt1, stmt2) ->
    let cond' = subst_expr subst cond in
    let stmt1' = subst_stmt subst stmt1 in
    let stmt2' = subst_stmt subst stmt2 in
    If (cond', stmt1', stmt2')
  | If1 (cond, stmt') ->
    let cond' = subst_expr subst cond in
    let stmt'' = subst_stmt subst stmt' in
    If1 (cond', stmt'')
  | Return e_opt ->
    Return (bind_opt (fun e -> Some (subst_expr subst e)) None e_opt)

(*Inline variables (that are to be substituted for) within a substitution*)
(*NOTE because of the way this function is implemented, the order of subst
  matters a great deal! Otherwise intermediate variables won't be inlined.*)
let rec inline_substvars_in_subst (acc : substitution) (subst : substitution) : substitution =
  match subst with
  | [] -> acc
  | (id, _) as id_expr :: rest ->
    if List.mem_assoc id acc then
      failwith "Substitution variable appears more than once in the substitution"
    else
      (*Update the substitution*)
      List.fold_right (fun (id', expr') acc ->
        (id', subst_expr subst expr') :: acc) rest []
      |> inline_substvars_in_subst (id_expr :: acc)

let subst_to_string ?st_opt:((st_opt : state option) = None)
      (subst : substitution) =
  let body =
    List.map (fun (id, expr) ->
    "(" ^ string_of_int id ^ ", " ^
    string_of_naasty_expression ~st_opt expr ^ ")") subst
  in "[" ^ String.concat "; " body ^ "]"

(*Erase declarations and assignments to inlined variables.
  NOTE we need to descend into For, If, etc*)
let rec erase_inlined (subst : substitution) (stmt : naasty_statement) : naasty_statement =
  match stmt with
  | Declaration (ty, _) ->
    begin
    match idx_of_naasty_type ty with
    | None ->
      failwith "Declaration must contain an identifier name, not just mention a type!"
    | Some idx ->
      if List.mem_assoc idx subst then Skip else stmt
    end
  | Seq (stmt1, stmt2) ->
    mk_seq (erase_inlined subst stmt1) (erase_inlined subst stmt2)
  | Assign (Var id, _) ->
    if List.mem_assoc id subst then Skip else stmt
  | For ((ty, cond, inc), body) ->
    let body' = erase_inlined subst body in
    For ((ty, cond, inc), body')
  | If (cond, stmt1, stmt2) ->
    let stmt1' = erase_inlined subst stmt1 in
    let stmt2' = erase_inlined subst stmt2 in
    If (cond, stmt1', stmt2')
  | If1 (cond, stmt') ->
    let stmt'' = erase_inlined subst stmt' in
    If1 (cond, stmt'')
  | _ -> stmt
