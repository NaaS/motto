(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.
*)

open General

type int_metadata = { signed : bool; precision : int }
let default_int_metadata = { signed = true; precision = 32 }
(*"identifier" is used for variables, constants and also field names.*)
type identifier = int
(*Variable-length types -- such as arrays -- can have their lengths either
  undefined, or fixed by a constant, or defined by some other field.*)
type vlen =
  | Undefined
  | Max of int
  | Dependent of identifier
    (*All identifiers have unique names, by construction. In this case, we're
      referring to a field name.*)
type type_identifier = int

(*FIXME where to store/use metadata for de/serialisers*)
type naasty_type =
  | Int_Type of identifier option * int_metadata
  | Bool_Type of identifier option
  | Char_Type of identifier option
    (*FIXME i'd prefer to use a "byte" type, but char seems to serve this
            purpose in C++*)
  | Array_Type of identifier option * naasty_type * vlen
  (*Tuples will be encoded as records*)
  | Record_Type of
      type_identifier * (*name for this type*)
      naasty_type list (*fields in the record*)
  | Unit_Type
    (*You cannot declare or name a value of unit type in NaaSty*)
  | UserDefined_Type of identifier option * type_identifier
    (*No identifier is provided if the UDT appears in the value type within
    a function type*)
  | Reference_Type of identifier option * naasty_type
    (*Pointer type*)
  | Size_Type of identifier option
    (*size_t*)
  | Static_Type of identifier option * naasty_type
  | Fun_Type of identifier(*Can't have anonymous functions*) *
                naasty_type * (*result type*)
                naasty_type list (*argument types*)

type naasty_expression =
  | Var of identifier
  | Int_Value of int
  | Bool_Value of bool
  | And of naasty_expression * naasty_expression
  | Or of naasty_expression * naasty_expression
  | Not of naasty_expression
  | Plus of naasty_expression * naasty_expression
  | Equals of naasty_expression * naasty_expression
  | GreaterThan of naasty_expression * naasty_expression
  | LessThan of naasty_expression * naasty_expression
  | Minus of naasty_expression * naasty_expression
  | Times of naasty_expression * naasty_expression
  | Mod of naasty_expression * naasty_expression
  | Quotient of naasty_expression * naasty_expression
  | Abs of naasty_expression
  | Call_Function of identifier * naasty_expression list
  | GEq of naasty_expression * naasty_expression

type naasty_statement =
    (*Should include function prototypes here?*)
  | Declaration of naasty_type (*FIXME would be nice to be able to initialise
                                       variables at declaration time.*)
  | Seq of naasty_statement * naasty_statement
  | Assign of identifier * naasty_expression
  | Increment of identifier * naasty_expression
  | For of (identifier * naasty_expression * naasty_statement) *
           naasty_statement
  | If of naasty_expression * naasty_statement * naasty_statement
  | If1 of naasty_expression * naasty_statement
  | Break
  | Continue
  | WriteToChan of identifier * identifier
  | ReadFromChan of identifier * identifier
  | Return of naasty_expression
  | Skip
  | Commented of naasty_statement * string

type naasty_function =
  identifier * naasty_type list * naasty_type * naasty_statement

type naasty_declaration =
    Type_Decl of naasty_type
  | Fun_Decl of naasty_function
  | Stmt of naasty_statement

type naasty_program = naasty_declaration list
