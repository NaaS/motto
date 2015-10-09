(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.
*)

open General

type int_metadata =
  { signed : bool;
    precision : int;
    hadoop_vint : bool;
  }
let default_int_metadata =
  { signed = true;
    precision = 32;
    hadoop_vint = false;
  }
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

type chan_direction =
  | Input
  | Output

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
  | Pointer_Type of identifier option * naasty_type
  | Size_Type of identifier option
    (*size_t*)
  | Static_Type of identifier option * naasty_type
  | Fun_Type of identifier(*Can't have anonymous functions*) *
                naasty_type * (*result type*)
                naasty_type list (*argument types*)
  | Chan_Type of identifier option * bool(*if this is an array of channels*) *
                 chan_direction * naasty_type

type template_parameter =
  | Type_Parameter of naasty_type
  | Term_Parameter of naasty_expression
and naasty_expression =
  | Var of identifier
  | Const of identifier
  | Int_Value of int
  | Char_Value of int
  | Bool_Value of bool
  | Array_Value of naasty_expression list (*NOTE values within an array must be of same type*)
  | Record_Value of (identifier * naasty_expression) list
  | Union_Value of identifier * naasty_expression
  | And of naasty_expression * naasty_expression
  | Or of naasty_expression * naasty_expression
  | Not of naasty_expression
  | Plus of naasty_expression * naasty_expression
  | Equals of naasty_expression * naasty_expression
  | Lt of naasty_expression * naasty_expression
  | Minus of naasty_expression * naasty_expression
  | Times of naasty_expression * naasty_expression
  | Mod of naasty_expression * naasty_expression
  | Quotient of naasty_expression * naasty_expression
  | Abs of naasty_expression
  | Call_Function of identifier * template_parameter list * naasty_expression list
  | GEq of naasty_expression * naasty_expression
  | Gt of naasty_expression * naasty_expression
  | Cast of naasty_type * naasty_expression
  | Dereference of naasty_expression
  | Field_In_Record of naasty_expression * naasty_expression
  | Address_of of naasty_expression
  | LEq of naasty_expression * naasty_expression
  | ArrayElement of naasty_expression * naasty_expression
  | Left_shift of naasty_expression * naasty_expression
  | Right_shift of naasty_expression * naasty_expression
  | Nullptr
    (*'Literal' consists of a string that's opaque to the IL, but that should
       make sense to the target language at the point of inclusion. By this I
       mean that, if we have 'Literal "a"' then "a" is either a keyword in the
       target language, or it is a symbol that's in scope.
       We use this for including expressions in the IL which we currently
       don't have a good way of encoding in the IL -- such as
       TaskEvent::OUT_OF_DATA.
       FIXME eventually we need a better way of handling this. We also need to
             register target-level functions such as "peek_channel" etc.
             I think we could use a map-based approach, as in the Functions module.*)
  | Literal of string

type naasty_statement =
    (*Should include function prototypes here?*)
  | Declaration of naasty_type * naasty_expression option
  | Seq of naasty_statement * naasty_statement
  | Assign of naasty_expression * naasty_expression
  | Increment of identifier * naasty_expression
  | For of (naasty_type * naasty_expression * naasty_statement) *
           naasty_statement
  | If of naasty_expression * naasty_statement * naasty_statement
  | If1 of naasty_expression * naasty_statement
  | Break
  | Continue
  | Return of naasty_expression option
  | Skip
  | Commented of naasty_statement * string
  | St_of_E of naasty_expression
  | Label of string * naasty_statement
  | GotoLabel of string
  | Switch of naasty_expression * (naasty_expression * naasty_statement) list

type naasty_function = {
  id : identifier;
  arg_tys : naasty_type list;
  ret_ty : naasty_type;
  body : naasty_statement;
}

type naasty_declaration =
    Type_Decl of naasty_type
  | Fun_Decl of naasty_function
  | Stmt of naasty_statement

type naasty_program = naasty_declaration list
