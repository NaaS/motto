(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.
*)

type int_metadata = { signed : bool; precision : int}

type naasty_type =
  | Int_Type of int_metadata
  | Bool_Type
  | String_Type

type identifier = string

type naasty_expression =
  | Int_Value of int


type naasty_statement =
    (*Should include function prototypes here?*)
  | Declaration of identifier * naasty_type
  | Seq of naasty_statement * naasty_statement
  | Assign of identifier * naasty_expression


type naasty_program = naasty_statement list

(*FIXME pretty printing*)
let string_of_program = ""

(*FIXME what's the metric for cost?
        maybe it should be a vector: memory use, computational cost, ...*)
let analyse_cost (prog : naasty_program) = 0
let parallelise (prog : naasty_program) : naasty_program list = []
let bound_cost (expense_per_unit : int) (prog : naasty_program) : naasty_program list = []


