(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.
*)

open General

let prog_indentation = 0
let no_indent = 0
let default_indentation = 2

type int_metadata = { signed : bool; precision : int}
type array_size = int
type type_identifier = int
type identifier = int

(* NOTE in the basic pretty-printing functions below we don't terminate with
        semicolons, since these functions could be used compositionally.*)

let ty_name i = "ty_" ^ string_of_int i
let id_name i = "id_" ^ string_of_int i

(*FIXME where to store/use metadata for de/serialisers*)
type naasty_type =
  | Int_Type of identifier option * int_metadata
  | Bool_Type of identifier option
  | String_Type of identifier option
  | Array_Type of identifier option * naasty_type * array_size option
  (*Tuples will be encoded as records*)
  | Record_Type of
(*
      identifier option * (*possible identifier this "type" -- this is only
                            relevant if the type spec occurs within another type
                            spec -- such as when we have a struct within a
                            struct.*)
*)
      type_identifier * (*name for this type*)
      naasty_type list (*fields in the record*)
  | Unit_Type
    (*You cannot declare or name a value of unit type in NaaSty*)
  | UserDefined_Type of identifier option * type_identifier
    (*No identifier is provided if the UDT appears in the value type within
    a function type*)
let rec string_of_naasty_type indent = function
  | Int_Type (id_opt, int_metadata) ->
    let prefix =
      if int_metadata.signed then "" else "u" in
    let suffix =
      (*FIXME should limit it to 16, 32, 64 etc*)
      string_of_int int_metadata.precision in
    indn indent ^
    prefix ^ "int" ^ suffix ^ "_t" ^
    bind_opt (fun i -> " " ^ id_name i) "" id_opt
  | Bool_Type id_opt ->
    indn indent ^
    "bool" ^
    bind_opt (fun i -> " " ^ id_name i) "" id_opt
  | String_Type id_opt ->
    (*FIXME ensure that we have '#include <cstring>' if we're to use this type
            (we're not using the standard C++ string type, according to examples
             i've seen)*)
    indn indent ^
    "string" ^
    bind_opt (fun i -> " " ^ id_name i) "" id_opt
    (*FIXME representation of string might lend itself better to C-style
      strings, to play nice with de/serialisers.*)
  | Array_Type (id_opt, naasty_type, array_size_opt) ->
    let size = match array_size_opt with
      | None -> ""
      | Some i -> string_of_int i in
    indn indent ^
    (*FIXME notation might be wrong -- the brackets enclosing the size might
            need to appear to the right of the variable name.*)
    string_of_naasty_type no_indent naasty_type ^ "[" ^ size ^ "]" ^
    bind_opt (fun i -> " " ^ id_name i) "" id_opt
  (*Tuples will be encoded as records*)
  | Record_Type (ty_ident, fields) ->
    (*Record types won't appear nested -- instead, the nested record will be
      pulled up to a global scope as a separate record type.*)
    let body =
      List.map (fun s ->
        string_of_naasty_type (indent + default_indentation) s ^ ";")
       fields
      |> String.concat "\n"
    in indn indent ^ "typedef " ^
    "struct " ^
    "{\n" ^ body ^ "\n" ^ indn indent ^ "}" ^
    " " ^ ty_name ty_ident
  | Unit_Type -> indn indent ^ "void"
  | UserDefined_Type (id_opt, ty_ident) ->
    indn indent ^
    ty_name ty_ident ^
    bind_opt (fun i -> " " ^ id_name i) "" id_opt

type naasty_expression =
  | Var of identifier
  | Int_Value of int
  | Plus of naasty_expression * naasty_expression
let rec string_of_naasty_expression = function
  | Int_Value i -> string_of_int i
  | Plus (e1, e2) ->
    "(" ^ string_of_naasty_expression e1 ^ ") + (" ^
    string_of_naasty_expression e2 ^ ")"
  | Var id -> id_name id

type naasty_statement =
    (*Should include function prototypes here?*)
  | Declaration of naasty_type
  | Seq of naasty_statement * naasty_statement
  | Assign of identifier * naasty_expression
  | For of (identifier * naasty_expression * naasty_statement) *
           naasty_statement
  | If of naasty_expression * naasty_statement * naasty_statement
  | Break
  | Continue
  | WriteToChan of identifier * identifier
  | ReadFromChan of identifier * identifier
  | Return of naasty_expression
let rec string_of_naasty_statement indent = function
  | Declaration ty ->
    (*NOTE assuming that types can only be defined globally,
           but they can be used in local variable declarations.*)
    string_of_naasty_type indent ty
  | Seq (stmt1, stmt2) ->
    string_of_naasty_statement indent stmt1 ^ ";\n" ^
    string_of_naasty_statement indent stmt2
  | Assign (id, e) ->
    indn indent ^ id_name id ^ " = " ^ string_of_naasty_expression e
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
    indn indent ^ "return (" ^ string_of_naasty_expression e ^ ")"

type naasty_function =
  identifier * naasty_type list * naasty_type * naasty_statement
let string_of_naasty_function indent (f_id, arg_types, res_type, body) =
  let arg_types_s =
   List.map (string_of_naasty_type indent) arg_types
   |> inter ", " in
  string_of_naasty_type indent res_type ^ " " ^ id_name f_id ^ " " ^
    "(" ^ arg_types_s ^ ") {\n" ^
    string_of_naasty_statement (indent + default_indentation) body ^ ";\n" ^
    "}"

type naasty_declaration =
    Type_Decl of naasty_type
  | Fun_Decl of naasty_function
  | Stmt of naasty_statement
let string_of_naasty_declaration indent = function
  | Type_Decl naasty_type -> string_of_naasty_type indent naasty_type
  | Fun_Decl naasty_function -> string_of_naasty_function indent naasty_function
  | Stmt naasty_statement -> string_of_naasty_statement indent naasty_statement

type naasty_program = naasty_declaration list
let string_of_naasty_program indent prog =
  List.map (string_of_naasty_declaration indent) prog
  |> String.concat ";\n"

;;
(*FIXME crude test*)
[
Type_Decl (Record_Type (8, [(Int_Type (Some 1, {signed = true; precision = 32}));
                 (Bool_Type (Some 2));
                 (String_Type (Some 3));
                 (Array_Type (Some 4,
                              Int_Type (None,
                                        {signed = false; precision = 64}),
                              Some 4))]));
Fun_Decl (0, [Bool_Type (Some 6); UserDefined_Type (Some 7, 8)], Int_Type (None, {signed = false; precision = 16}),
          Seq (Declaration (Int_Type (Some 1, {signed = false; precision = 16})),
               Seq (Assign (1, Int_Value 5),
                    Return (Var 1))))
]
|> string_of_naasty_program prog_indentation
|> print_endline
;;
