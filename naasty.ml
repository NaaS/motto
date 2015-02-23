(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.
*)

type int_metadata = { signed : bool; precision : int}
type string_metadata = { max_size : int option } (*FIXME include other metadata*)
type array_size = int
type field_id = int

type naasty_type =
  | Int_Type of int_metadata
  | Bool_Type
  | String_Type of string_metadata
  | Byte_Type
  | Array_Type of naasty_type * array_size option
  (*Tuples will be encoded as records*)
  | Record_Type of (field_id * naasty_type) list

(*Using de Bruijn binding at this level*)
type identifier = int

type naasty_expression =
  | Int_Value of int

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

type naasty_function =
  identifier * naasty_type * naasty_type * naasty_statement

type naasty_declaration =
    Type_Decl of naasty_type
  | Fun_Decl of naasty_function
  | Stmt of naasty_statement

type naasty_program = naasty_declaration list

(*FIXME pretty printing*)
let string_of_program = ""
