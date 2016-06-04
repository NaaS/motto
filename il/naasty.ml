(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
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
  | Literal_Type of identifier option * string
  | Union_Type of
      (*FIXME this is a very primitive type. Would be nice to be able to have
              families of ILs, some more primitive than others.*)
      type_identifier * (*name for this type*)
      naasty_type list (*fields in the union*)

let is_literal_type = function
  | Literal_Type _ -> true
  | _ -> false

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
       FIXME eventually we need a better way of handling this. At least, we
             should provide some type information for a Literal, to ensure
             consistency at compile time.
             We should also somehow register target-level functions such as
             "peek_channel" etc.  I think we could use a map-based approach, as
             in the Functions module.*)
  | Literal of string

type naasty_statement =
    (*Should include function prototypes here?*)
  | Declaration of naasty_type(*the declared variable and its type*) *
                   naasty_expression option(*optional initial value*) *
                   bool(*whether we want this declaration to be emitted, or if
                         it's only to be used for sanity-checking (so that the
                         compiler knows that all variables have been handled
                         correctly, even if they're assumed to be in scope when
                         emitted).*)
  | Seq of naasty_statement * naasty_statement
  | Assign of naasty_expression * naasty_expression
  | Increment of identifier * naasty_expression
  | For of ((naasty_type(*cursor variable*) *
             naasty_expression(*cursor's initial value*)) *
            naasty_expression(*termination predicate*) *
            naasty_statement(*statement run after each execution of loop body*)) *
            naasty_statement(*body of loop*)
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
  | Literal_Type (id_opt, s) ->
    if id_opt = None then
      Literal_Type (Some idx, s)
    else failwith "Cannot set an already-set index"
