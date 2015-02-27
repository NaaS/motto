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
    string_of_naasty_type ~st_opt indent naasty_type ^
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
  List.map (string_of_naasty_declaration ~st_opt indent) prog
  |> String.concat ";\n"

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
