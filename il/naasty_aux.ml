(*
   Supporting definitions and functions for the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty
open State


exception Naasty_aux_Exc of string * state option

type lbl = string

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
      | None ->
        raise (Naasty_aux_Exc
                 ("Could not resolve idx " ^ string_of_int i ^
                  " in " ^ scope_to_str scope ^ " scope", st_opt))
      | Some name -> name
    end
let no_prefix = ""
let ty_prefix = "ty_"
let ty_name = resolve_idx Type ty_prefix
let id_prefix = "id_"
let id_name = resolve_idx (Term Undetermined) id_prefix

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
        "std::vector<Buffer *> &" ^
        bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
      | false ->
        "Buffer &"(*FIXME change syntax -- we don't have singleton
                          channels in the C++ implementation examples*) ^
        bind_opt (fun i -> " " ^ id_name st_opt i) "" id_opt
    end

let rec size_of_naasty_expression : naasty_expression -> int = function
  | Minus (Int_Value 0, Int_Value _)
  | Var _
  | Const _
  | Int_Value _
  | Char_Value _
  | Bool_Value _
  | Array_Value _
  | Record_Value _
  | Union_Value _
  | Call_Function (_, _, _)
  | Nullptr
  | Field_In_Record (_, _)
  | Literal _ -> 1
  | Dereference e
  | Address_of e ->
    (*No need to add 1 to the size, otherwise we'll always have
      brackets around simple expressions like "&size"*)
    size_of_naasty_expression e
  | Cast (_, e)
  | Not e
  | Abs e -> 1 + size_of_naasty_expression e
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
  | LEq (e1, e2)
  | Left_shift (e1, e2)
  | Right_shift (e1, e2) ->
    1 + size_of_naasty_expression e1 + size_of_naasty_expression e2
  | ArrayElement (e1, e2) ->
    let e1_size = size_of_naasty_expression e1 in
    let e2_size = size_of_naasty_expression e2 in
    if e1_size = e2_size && e1_size = 1 then
      1
    else
      1 + e1_size + e2_size

let rec string_of_template_parameter ?st_opt:((st_opt : state option) = None)
          (tp : template_parameter) : string =
  match tp with
  | Type_Parameter ty ->
    string_of_naasty_type ~st_opt 0 ty
  | Term_Parameter e ->
    string_of_naasty_expression ~st_opt e
    |> fst
and string_of_naasty_expression ?st_opt:((st_opt : state option) = None)
          (e : naasty_expression) : string * bool(*if bracketed*) =
  let e_s =  
    match e with
    | Minus (Int_Value 0, Int_Value n) -> "-" ^ string_of_int n
    | Int_Value i -> string_of_int i
    | Plus (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " + " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Var id -> id_name st_opt id
    | Const id -> id_name st_opt id
    | Call_Function (id, template_params, es) ->
      let template_params_s =
        if template_params = [] then ""
        else
          let body =
          List.map (string_of_template_parameter ~st_opt) template_params
          |> String.concat ", "
          in "<" ^ body ^  ">" in
      let arg_s =
        List.map (string_of_naasty_expression ~st_opt) es
        |> List.map fst
        |> String.concat ", " in
      id_name st_opt id ^ template_params_s ^ " (" ^ arg_s ^ ")"
    | GEq (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " >= " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Gt (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " > " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Cast (ty, e) ->
      "(" ^ string_of_naasty_type no_indent ~st_opt ty ^ ")" ^
      fst (string_of_naasty_expression ~st_opt e)
    | Field_In_Record (Dereference record_ref, field) ->
      (*NOTE this syntactic sugaring is handled directly*)
      fst (string_of_naasty_expression ~st_opt record_ref) ^
      "->" ^
      fst (string_of_naasty_expression ~st_opt field)
    | Dereference e ->
      "*" ^ fst (string_of_naasty_expression ~st_opt e)
    | Field_In_Record (record, field) ->
      fst (string_of_naasty_expression ~st_opt record) ^
      "." ^
      fst (string_of_naasty_expression ~st_opt field)
    | Address_of e ->
      "&" ^ fst (string_of_naasty_expression ~st_opt e)
    | And (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " && " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Or (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " || " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Not e ->
      "!" ^ fst (string_of_naasty_expression ~st_opt e)
    | Equals (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " == " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Lt (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " < " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Minus (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " - " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Times (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " * " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | LEq (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " <= " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | ArrayElement (arr, idx) ->
      fst (string_of_naasty_expression ~st_opt arr) ^ "[" ^
      fst (string_of_naasty_expression ~st_opt idx) ^ "]"
    | Left_shift (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " << " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Right_shift (e1, e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " >> " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Bool_Value (b) -> 
      string_of_bool b
    | Abs (e) ->
      "abs (" ^ fst (string_of_naasty_expression ~st_opt e) ^ ")" 
    | Mod (e1,e2) ->
      fst (string_of_naasty_expression ~st_opt e1) ^ " % " ^
      fst (string_of_naasty_expression ~st_opt e2)
    | Record_Value fields ->
      let fields_s =
        List.map (fun (id, e) ->
          "." ^ id_name st_opt id ^ " = " ^
          fst (string_of_naasty_expression ~st_opt e)) fields
         |> String.concat ", " in
      "{" ^ fields_s ^ "}" 
    | Nullptr -> "nullptr"
    | Literal s -> s in
  if size_of_naasty_expression e = 1 then
    e_s, false
  else
    "(" ^ e_s ^ ")", true

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
      | Some e -> " = " ^ fst (string_of_naasty_expression ~st_opt e)
    in string_of_naasty_type ~st_opt indent ty ^ definition ^ terminal
  | Seq (stmt1, stmt2) ->
    string_of_naasty_statement ~st_opt indent stmt1 ^ "\n" ^
    string_of_naasty_statement ~st_opt indent stmt2
  | Assign (lvalue, e) ->
    indn indent ^ fst (string_of_naasty_expression ~st_opt lvalue) ^ " = " ^
    fst (string_of_naasty_expression ~st_opt e) ^ terminal
  | Increment (id, e) ->
    indn indent ^ id_name st_opt id ^ " += " ^
    fst (string_of_naasty_expression ~st_opt e) ^ terminal
  | For (((id, id_init), condition, increment), body) ->
    (*FIXME check if the target syntax is correct*)
    indn indent ^ "for (" ^
    string_of_naasty_type ~st_opt no_indent id ^ " = " ^
    fst (string_of_naasty_expression ~st_opt id_init) ^ "; " ^
    fst (string_of_naasty_expression ~st_opt condition) ^ "; " ^
    string_of_naasty_statement ~st_opt no_indent ~print_semicolon:false increment ^ ") {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) body ^
    "\n" ^ indn indent ^ "}"
  | If (e, stmt1, stmt2) ->
    let cond_s =
      match string_of_naasty_expression ~st_opt e with
      | s, true -> s
      | s, false ->
        (*Ensure that condition is bracketed*)
        "(" ^ s ^ ")" in
    indn indent ^ "if " ^ cond_s ^ " {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) stmt1 ^
    "\n" ^
    indn indent ^ "} else {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) stmt2 ^
    "\n" ^
    indn indent ^ "}"
  | If1 (e, stmt1) ->
    let cond_s =
      match string_of_naasty_expression ~st_opt e with
      | s, true -> s
      | s, false ->
        (*Ensure that condition is bracketed*)
        "(" ^ s ^ ")" in
    indn indent ^ "if " ^ cond_s ^ " {\n" ^
    string_of_naasty_statement ~st_opt (indent + indentation) stmt1 ^
    "\n" ^
    indn indent ^ "}"
  | Break -> indn indent ^ "break" ^ terminal
  | Continue -> indn indent ^ "continue" ^ terminal
(*
  | PeekChan of identifier
	| ConsumeChan of identifier
	| ForwardChan of identifier * identifier
*)
  | Return e_opt ->
    let f e = " " ^ fst (string_of_naasty_expression ~st_opt e) in
    indn indent ^ "return" ^  bind_opt f "" e_opt ^ terminal
  | Skip -> indn indent ^ "/*skip*/"
  | Commented (Skip, comment) ->
    (*Simply print the comment*)
    indn indent ^ "// " ^ comment
  | Commented (stmt, comment) ->
    (*First print the statement, then the comment*)
    string_of_naasty_statement ~st_opt indent stmt ^ " // " ^ comment
  | St_of_E e ->
    indn indent ^ fst (string_of_naasty_expression ~st_opt e) ^ terminal
  | Label (lbl, stmt) ->
    indn indent ^ lbl ^ ": " ^
    string_of_naasty_statement ~st_opt no_indent stmt ^ terminal
  | GotoLabel lbl ->
    indn indent ^ "goto " ^ lbl ^ terminal
  | Switch (e, cases) ->
    let e_to_str e =
      (*FIXME this function seems to be generally useful -- lift to higher
              scope?*)
      match string_of_naasty_expression ~st_opt e with
      | s, true -> s
      | s, false ->
        (*Ensure that condition is bracketed*)
        "(" ^ s ^ ")" in
    let case_s (e, stmt) =
      let e_s, _ = string_of_naasty_expression ~st_opt e in
      indn (indent + default_indentation) ^ "case " ^ e_s ^ ": {\n" ^
      string_of_naasty_statement ~st_opt (indent + (2 * default_indentation)) stmt ^
      "\n" ^
      indn (indent + 2 * default_indentation) ^ "} break"(*FIXME add "break" to Naasty*) ^
      terminal in
    indn indent ^ "switch " ^ e_to_str e ^ " {\n" ^
    String.concat "\n" (List.map case_s cases) ^
    "\n" ^ indn indent ^ "}"

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

  Furthermore we don't update the labels of the src/IL types associated with
  that name. For a safer version of this function, see extend_scope_unsafe.

  NOTE we don't check for clashes! thus the _unsafe prefix*)
let extend_scope_unsafe' (scope : scope) (st : state)
      ?src_ty_opt:(src_ty_opt = None) ?ty_opt:(ty_opt = None)
      ?dependency_index:(dependency_index = false)
      ?decl_scope_opt:(decl_scope_opt = None) (id : string) : Naasty.identifier * state =
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
  | Term ik ->
    (*Must not add polymorphic constants to the symbol table.*)
    assert (match src_ty_opt with
            | Some (Crisp_syntax.Undefined _) -> false
            | _ -> true);
    (st.next_symbol,
     let metadata =
     {
       declaration_scope = decl_scope_opt;
       dependency_index = dependency_index;
       source_type = src_ty_opt;
       naasty_type = ty_opt';
       identifier_kind = ik;
       channel_id = None;
     } in
     { st with
       term_symbols = (id, st.next_symbol, metadata) :: st.term_symbols;
       next_symbol = 1 + st.next_symbol;
     })

(*Extends a scope by adding a mapping between a name and an index.

  Calls extend_scope_unsafe' to get the basic work done, then it updates
  labels of the src/IL types to ensure consistency.

  NOTE we don't check for clashes! thus the _unsafe prefix*)
let extend_scope_unsafe (scope : scope) (st : state)
      ?src_ty_opt:(src_ty_opt = None) ?ty_opt:(ty_opt = None)
      ?dependency_index:(dependency_index = false)
      ?decl_scope_opt:(decl_scope_opt = None) (id : string) : Naasty.identifier * state =
  let (idx, st) =
    extend_scope_unsafe' scope st ~src_ty_opt ~ty_opt ~dependency_index
      ~decl_scope_opt id in
  (*Now update the labels in the types that are associated with id, in the
    newly-updated stage.*)
  let st' =
     { st with
       term_symbols =
         List.map (fun ((id', idx', md) as data) ->
           if not (id' = id || idx' = idx) then data
           else
             begin
               if !Config.cfg.Config.verbosity > 1 then
                 print_endline ("Have " ^ id' ^ "=" ^ id ^ " and " ^
                             string_of_int idx' ^ "=" ^ string_of_int idx);
(*FIXME currently disabled this since it's failed by how we handle TaskEvent
        at present.
               assert (id' = id && idx' = idx);*)
               let md' =
               { md with
                 source_type =
                   bind_opt (fun ty ->
                     Crisp_syntax_aux.set_empty_label ty
                     |> Crisp_syntax_aux.update_empty_label id
                     |> (fun x -> Some x)) None md.source_type;
                 naasty_type =
                   bind_opt (fun ty ->
                     set_empty_identifier ty
                     |> update_empty_identifier idx
                     |> (fun x -> Some x)) None md.naasty_type;
               } in
               (id', idx', md')
             end) st.term_symbols } in
  (idx, st')

(*Adds a fresh identifier to the scope, based on a specific prefix, to which
  we concatenate a numeric suffix/index*)
let mk_fresh (scope : scope) ?src_ty_opt:(src_ty_opt = None)
      ?ty_opt:(ty_opt = None) (id : string) (min_idx : int) (st : state) :
  string * Naasty.identifier * state =
  if min_idx < 0 then
    failwith "min_idx must be non-negative"
  else if !Config.cfg.Config.naive_internal_naming then
    let idx, st' =
      match lookup_name scope st id with
      | None ->
        extend_scope_unsafe scope st ~src_ty_opt ~ty_opt id
      | Some idx -> idx, st
    in (id, idx, st')
  else
    let idx = ref min_idx in
    while (lookup_name scope st (id ^ string_of_int !idx) <> None) do
      idx := 1 + !idx
    done;
    let name = id ^ string_of_int !idx in
    let (idx, st') = extend_scope_unsafe scope st ~src_ty_opt ~ty_opt name
    in (name, idx, st')

(*Indicates if a name is fresh in either scope*)
let is_fresh (id : string) (st : state) : bool =
  lookup_name (Term Undetermined) st id = None && lookup_name Type st id = None

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
        let scope = if type_mode then Type else Term Value in
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
          match lookup_name (Term Undetermined) st local_name with
          | None ->
            extend_scope_unsafe (Term Value) st local_name
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
  | Const id ->
    let id', st' =
      substitute fresh names false id st id (fun x -> x)
    in (Const id', st')
  | Int_Value _
  | Char_Value _
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
  | Call_Function (id, tps, es) ->
    let id', st' =
      substitute fresh names false id st id (fun x -> x) in
    let es', st'' =
      fold_map ([], st') (instantiate_expression fresh names) es in
    let tps', st''' =
      let instantiate_template_parameter st tp =
        match tp with
        | Type_Parameter ty ->
          let ty', st' = instantiate_type fresh names st ty in
          (Type_Parameter ty'), st'
        | Term_Parameter e ->
          let e', st' = instantiate_expression fresh names st e in
          (Term_Parameter e'), st' in
      fold_map ([], st'') instantiate_template_parameter tps
    in (Call_Function (id', tps', es'), st''')
  | GEq (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> GEq (e1', e2'))
  | Gt (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Gt (e1', e2'))
  | Cast (ty, e) ->
    let ty', st' = instantiate_type fresh names st ty in
    let e', st'' =
      instantiate_expression fresh names st' e
    in (Cast (ty', e'), st'')
  | Dereference e -> unary_op_inst e (fun e' -> Dereference e')
  | Field_In_Record (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Field_In_Record (e1', e2'))
  | Address_of e -> unary_op_inst e (fun e' -> Address_of e')
  | ArrayElement (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> ArrayElement (e1', e2'))
  | Left_shift (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Left_shift (e1', e2'))
  | Right_shift (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Right_shift (e1', e2'))
  | Array_Value es ->
    let es', st' = fold_map ([], st) (instantiate_expression fresh names) es
    in (Array_Value es', st')
  | Record_Value fields ->
    let fields', st' = fold_map ([], st) (fun st (id, e) ->
      let e', st' = instantiate_expression fresh names st e in
      ((id, e'), st')) fields
    in (Record_Value fields', st')
  | Union_Value (id, e') -> unary_op_inst e' (fun e' -> Union_Value (id, e'))

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
  | For (((id, id_init), condition, increment), body) ->
    let id', st' = instantiate_type fresh names st id in
    let (id_init', st') = instantiate_expression fresh names st' condition in
    let (condition', st'') = instantiate_expression fresh names st' condition in
    let (increment', st''') = instantiate_statement fresh names st'' increment in
    let (body', st4) = instantiate_statement fresh names st''' body
    in (For (((id', id_init'), condition', increment'), body'), st4)
  | Label (lbl, stmt) ->
    let (stmt', st') = instantiate_statement fresh names st stmt
    in (Label (lbl, stmt'), st')
  | GotoLabel lbl -> (GotoLabel lbl, st)

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
    Field_In_Record (Dereference (Var record), Var field)
  | field :: rest ->
    let record = nested_fields rest in
    Field_In_Record (Dereference record, Var field)
  | _ ->
    failwith "There needs to be at least one record and one field: field_idents needs to contain at least two items."

(*Checks if an expression contains a Functor_App. This is used by the inliner
  to avoid removing function applications, in case we're relying on their
  side-effects.
  NOTE a better approach would involve checking inside those functions to
       see whether we might actually have side-effects.
*)
let rec contains_functor_app : naasty_expression -> bool = function
  | Call_Function (_, _, _) -> true
  | Var _
  | Const _
  | Int_Value _
  | Char_Value _
  | Bool_Value _ -> false
  | Array_Value es ->
    List.fold_right (fun e acc ->
      acc || contains_functor_app e) es false
  | Record_Value fields ->
    List.fold_right (fun (_, e) acc ->
      acc || contains_functor_app e) fields false
  | Union_Value (_, e)
  | Not e
  | Abs e
  | Dereference e
  | Address_of e
  | Cast (_, e) -> contains_functor_app e
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
  | Field_In_Record (e1, e2)
  | LEq (e1, e2)
  | ArrayElement (e1, e2)
  | Left_shift (e1, e2)
  | Right_shift (e1, e2) ->
    contains_functor_app e1 || contains_functor_app e2

(*Add a symbol (to a given scope) unless it's already been added to the given scope*)
let add_symbol (label : label) (scope : scope)
      ?src_ty_opt:(src_ty_opt : type_value option = None)
      ?ty_opt:(ty_opt : naasty_type option = None)
      (st : state) : int * state =
  match lookup_name scope st label with
  | Some id -> id, st
  | None ->
    (*Declare the symbol*)
    extend_scope_unsafe ~src_ty_opt ~ty_opt scope st label

(*Like add_symbol, but also ensures that its type is added (as a user-defined
  type)*)
let add_usertyped_symbol (typ_name : label) (term_name : label) (st : state) : int * int * state =
  match lookup_name Type st typ_name, lookup_name (Term Value) st term_name with
  | Some ty, Some te -> ty, te, st
  | None, None ->
    (*Declare them both*)
    let (type_id, st') = extend_scope_unsafe Type st typ_name in
    let (var_id, st'') =
      extend_scope_unsafe (Term Value) st'
        ~ty_opt:(Some (UserDefined_Type (None, type_id))) term_name in
    type_id, var_id, st''
  | _, _ ->
    failwith ("Impossible: '" ^ typ_name ^ "' type and '" ^ term_name ^
              "' variable not both declared")

(*Break up a statement into consituent statement sequence, if possible.*)
let rec unconcat (stmt : naasty_statement) (sts : naasty_statement list) : naasty_statement list =
  match stmt with
  | Seq (stmt1, stmt2) ->
    unconcat stmt1 sts @ unconcat stmt2 sts
  | _ -> List.rev (stmt :: sts)

let rec unconcat (source : naasty_statement list) (result : naasty_statement list) : naasty_statement list =
  match source with
  | [] -> List.rev result
  | (Seq (stmt1, stmt2) :: rest) -> unconcat (stmt1 :: stmt2 :: rest) result
  | (x :: xs) -> unconcat xs (x :: result)
