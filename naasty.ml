(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.
*)

open General

let prog_indentation = 0
let default_indentation = 2

type int_metadata = { signed : bool; precision : int}
type array_size = int
type field_id = int
type type_identifier = int
type identifier = int

(* NOTE in the basic pretty-printing functions below we don't terminate with
        semicolons, since these functions could be used compositionally.*)

let ty_name i = "ty_" ^ string_of_int i
let id_name i = "id_" ^ string_of_int i

(*FIXME where to store/use metadata for de/serialisers*)
type naasty_type =
  | Int_Type of int_metadata
  | Bool_Type
  | String_Type
  | Array_Type of naasty_type * array_size option
  (*Tuples will be encoded as records*)
  | Record_Type of type_identifier * (field_id * naasty_type) list
  | Unit_Type
  | UserDefined_Type of type_identifier * naasty_type option
let rec string_of_naasty_type declaration = function
  | Int_Type int_metadata ->
    let prefix =
      if int_metadata.signed then "" else "u" in
    let suffix =
      (*FIXME should limit it to 16, 32, 64 etc*)
      string_of_int int_metadata.precision in
    prefix ^ "int" ^ suffix ^ "_t"
  | Bool_Type -> "bool"
  | String_Type ->
    (*FIXME ensure that we have '#include <cstring>' if we're to use this type
            (we're not using the standard C++ string type, according to examples
             i've seen)*)
    "string"
    (*FIXME representation of string might lend itself better to C-style
      strings, to play nice with de/serialisers.*)
  | Array_Type (naasty_type, array_size_opt) ->
    let size = match array_size_opt with
      | None -> ""
      | Some i -> string_of_int i in
    string_of_naasty_type false naasty_type ^ "[" ^ size ^ "]"
  (*Tuples will be encoded as records*)
  | Record_Type (ty_id, fields) ->
    let body =
      if not declaration then ""
      else
        let fields_str =
          List.map (fun (f_id, ty) ->
            string_of_naasty_type false ty ^ " " ^ id_name f_id) fields
          |> inter "; "
        in " { " ^ fields_str ^ " }"
    in "struct " ^ ty_name ty_id ^ body
  | Unit_Type -> "void"
  | UserDefined_Type (ty_id, tydef_opt) ->
    if declaration then
      begin
        match tydef_opt with
        | None -> failwith "Missing type definition"
        | Some ty ->
          "typedef " ^
          string_of_naasty_type false ty ^ " " ^
          ty_name ty_id
      end
    else ty_name ty_id

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
  | Declaration of identifier * naasty_type
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
  | Declaration (id, ty) ->
    (*NOTE assuming that types can only be declared globally*)
    indn indent ^ string_of_naasty_type false ty ^ " " ^ id_name id
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
let string_of_naasty_function (f_id, arg_types, res_type, body) =
  let arg_types_s =
   List.map (string_of_naasty_type false) arg_types
   |> inter ", " in
  string_of_naasty_type false res_type ^ " " ^ id_name f_id ^
    "(" ^ arg_types_s ^ ") {\n" ^
    string_of_naasty_statement default_indentation body ^ "\n}"

type naasty_declaration =
    Type_Decl of naasty_type
  | Fun_Decl of naasty_function
  | Stmt of naasty_statement
let string_of_naasty_declaration = function
  | Type_Decl naasty_type -> string_of_naasty_type true naasty_type
  | Fun_Decl naasty_function -> string_of_naasty_function naasty_function
  | Stmt naasty_statement -> string_of_naasty_statement prog_indentation naasty_statement

type naasty_program = naasty_declaration list
let string_of_program prog =
  List.map string_of_naasty_declaration prog
  |> inter "; "

;;
(*FIXME crude test*)
Record_Type (0, [(1, Int_Type {signed = true; precision = 32});
                 (2, Bool_Type);
                 (3, String_Type);
                 (4, Array_Type (Int_Type {signed = false; precision = 64},
                                 Some 4))])
|> string_of_naasty_type true
|> print_endline
;;
Fun_Decl (0, [], Int_Type {signed = false; precision = 16},
          Seq (Declaration (1, Int_Type {signed = false; precision = 16}),
               Seq (Assign (1, Int_Value 5),
                    Return (Var 1))))

|> string_of_naasty_declaration
|> print_endline
;;
