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
let ty_prefix = "ty_"
let ty_name = resolve_idx Type ty_prefix
let id_prefix = "id_"
let id_name = resolve_idx Term id_prefix

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
  | Reference_Type (id_opt, naasty_type) ->
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

let rec string_of_naasty_expression ?st_opt:((st_opt : state option) = None) = function
  | Int_Value i -> string_of_int i
  | Plus (e1, e2) ->
    "(" ^ string_of_naasty_expression ~st_opt e1 ^ ") + (" ^
    string_of_naasty_expression ~st_opt e2 ^ ")"
  | Var id -> id_name st_opt id
  | _ -> failwith "TODO"

let rec string_of_naasty_statement ?st_opt:((st_opt : state option) = None) indent = function
  | Declaration ty ->
    (*NOTE assuming that types can only be defined globally,
           but they can be used in local variable declarations.*)
    string_of_naasty_type ~st_opt indent ty
  | Seq (stmt1, stmt2) ->
    string_of_naasty_statement ~st_opt indent stmt1 ^ ";\n" ^
    string_of_naasty_statement ~st_opt indent stmt2
  | Assign (id, e) ->
    indn indent ^ id_name st_opt id ^ " = " ^ string_of_naasty_expression ~st_opt e
(*
  | For of (identifier * naasty_expression * naasty_statement) *
           naasty_statement
  | If of naasty_expression * naasty_statement * naasty_statement
*)
  | Break -> indn indent ^ "break"
  | Continue -> indn indent ^ "continue"
(*
  | WriteToChan of identifier * identifier
  | ReadFromChan of identifier * identifier
*)
  | Return e ->
    indn indent ^ "return (" ^ string_of_naasty_expression ~st_opt e ^ ")"
  | _ -> failwith "TODO"

let string_of_naasty_function ?st_opt:((st_opt : state option) = None) indent (f_id, arg_types, res_type, body) =
  let arg_types_s =
   List.map (string_of_naasty_type ~st_opt indent) arg_types
   |> String.concat ", " in
  string_of_naasty_type ~st_opt indent res_type ^ " " ^ id_name st_opt f_id ^ " " ^
    "(" ^ arg_types_s ^ ") {\n" ^
    string_of_naasty_statement ~st_opt (indent + default_indentation) body ^ ";\n" ^
    "}"

let string_of_naasty_declaration ?st_opt:((st_opt : state option) = None) indent = function
  | Type_Decl naasty_type -> string_of_naasty_type ~st_opt indent naasty_type
  | Fun_Decl naasty_function -> string_of_naasty_function ~st_opt indent naasty_function
  | Stmt naasty_statement -> string_of_naasty_statement ~st_opt indent naasty_statement

let string_of_naasty_program ?st_opt:((st_opt : state option) = None) indent prog =
  prog
  |> List.map
       (fun decl ->
          string_of_naasty_declaration ~st_opt indent decl
          |> fun s -> s ^ ";")
  |> String.concat "\n"

(*Extends a scope by adding a mapping between a name and an index.
  NOTE we don't check for clashes! thus the _unsafe prefix*)
let extend_scope_unsafe (scope : scope) (st : state) (id : string) : Naasty.identifier * state =
  match scope with
  | Type ->
    (st.next_symbol,
     { st with
       type_symbols = (id, st.next_symbol, None) :: st.type_symbols;
       next_symbol = 1 + st.next_symbol;
     })
  | Term ->
    (st.next_symbol,
     { st with
       term_symbols = (id, st.next_symbol, None) :: st.term_symbols;
       next_symbol = 1 + st.next_symbol;
     })

(*Adds a fresh identifier to the scope, based on a specific prefix, to which
  we concatenate a numeric suffix/index*)
let mk_fresh (scope : scope) (id : string) (min_idx : int) (st : state) :
  string * Naasty.identifier * state =
  if min_idx < 0 then
    failwith "min_idx must be non-negative"
  else
    let idx = ref min_idx in
    while (lookup_name scope st (id ^ string_of_int !idx) <> None) do
      idx := 1 + !idx
    done;
    let name = id ^ string_of_int !idx in
    let (idx, st') = extend_scope_unsafe scope st name
    in (name, idx, st')

(*Instantiates a naasty_type scheme with a set of names*)
let rec instantiate (fresh : bool) (names : string list) (st : state)
      (scheme : naasty_type) : naasty_type * state =
  let substitute' (type_mode : bool) scheme st id f =
    if id > 0 then (scheme, st)
    else if id = 0 then
      failwith "Template placeholder cannot be 0 -- this value is undefined."
    else
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
      in (f id', st') in
  let substitute (type_mode : bool) scheme st id_opt f =
    match id_opt with
    | None -> (scheme, st)
    | Some id ->
        substitute' type_mode scheme st id f
  in match scheme with
  | Int_Type (id_opt, int_metadata) ->
    substitute false scheme st id_opt (fun id' ->
      Int_Type (Some id', int_metadata))
  | Bool_Type id_opt ->
    substitute false scheme st id_opt (fun id' ->
      Bool_Type (Some id'))
  | Char_Type id_opt ->
    substitute false scheme st id_opt (fun id' ->
      Char_Type (Some id'))
  | Array_Type (id_opt, naasty_type, array_size) ->
    let naasty_type', st' =
      instantiate fresh names st naasty_type in
    if naasty_type' = naasty_type then
      begin
        assert (st = st');
        substitute false scheme st id_opt (fun id' ->
        Array_Type (Some id', naasty_type, array_size))
      end
    else
      Array_Type (id_opt, naasty_type', array_size)
      |> instantiate fresh names st'
  | Record_Type (ty_ident, fields) ->
    let ty_ident', st' =
      substitute' true ty_ident st ty_ident (fun x -> x) in
    let fields', st'' =
      fold_map ([], st') (instantiate fresh names) fields in
    (Record_Type (ty_ident', fields'), st'')
  | Unit_Type -> (Unit_Type, st)
  | UserDefined_Type (id_opt, ty_ident) ->
    let ty_ident', st' =
      substitute' true ty_ident st ty_ident (fun x -> x) in
    let scheme' = UserDefined_Type (id_opt, ty_ident') in
    substitute false scheme' st' id_opt (fun id' ->
      UserDefined_Type (Some id', ty_ident'))
  | Reference_Type (id_opt, naasty_type) ->
    let naasty_type', st' =
      instantiate fresh names st naasty_type in
    if naasty_type' = naasty_type then
      begin
        assert (st = st');
        substitute false scheme st id_opt (fun id' ->
        Reference_Type (Some id', naasty_type))
      end
    else
      Reference_Type (id_opt, naasty_type')
      |> instantiate fresh names st'
  | Size_Type id_opt ->
    substitute false scheme st id_opt (fun id' ->
      Size_Type (Some id'))
  | Static_Type (id_opt, naasty_type) ->
    let naasty_type', st' =
      instantiate fresh names st naasty_type in
    if naasty_type' = naasty_type then
      begin
        assert (st = st');
        substitute false scheme st id_opt (fun id' ->
        Static_Type (Some id', naasty_type))
      end
    else
      Static_Type (id_opt, naasty_type')
      |> instantiate fresh names st'
  | Fun_Type (id, res_ty, arg_tys) ->
    let id', st' =
      substitute' false id st id (fun x -> x) in
    let res_ty', st'' =
      instantiate fresh names st' res_ty in
    let arg_tys', st''' =
      fold_map ([], st'') (instantiate fresh names) arg_tys in
    (Fun_Type (id', res_ty', arg_tys'), st''')

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
  List.map (fun recipient -> Assign (recipient, definiens)) recipients

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
  | [] ->
    (*We could return Skip here, but for the time being i prefer failing since
      i don't think we should be getting empty statement lists..*)
    failwith "Statement concat must be applied to at least one statement."
  | [s] -> s
  | [s1; s2] -> mk_seq s1 s2
  | s1 :: s2 :: rest ->
    concat rest
    |> mk_seq s2
    |> mk_seq s1

;;
(*FIXME crude test*)
[
Type_Decl (Record_Type (8, [(Int_Type (Some 1, {signed = true; precision = 32}));
                 (Bool_Type (Some 2));
                 (Char_Type (Some 3));
                 (Array_Type (Some 4,
                              Int_Type (None,
                                        {signed = false; precision = 64}),
                              Max 4))]));
Fun_Decl (0, [Bool_Type (Some 6); UserDefined_Type (Some 7, 8)], Int_Type (None, {signed = false; precision = 16}),
          Seq (Declaration (Int_Type (Some 1, {signed = false; precision = 16})),
               Seq (Assign (1, Int_Value 5),
                    Return (Var 1))))
]
|> string_of_naasty_program prog_indentation
|> print_endline
;;
