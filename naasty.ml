(*
   Implementation of the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015

   Flick programs are translated into NaaSty, en route to becoming C++&libNaaS
   programs. NaaSty can be regarded as an embedded of part of C++, and of
   libNaaS, into OCaml.
*)

type int_metadata = { signed : bool; precision : int}

type array_size = int

type naasty_type =
  | Int_Type of int_metadata
  | Bool_Type
  | String_Type
  | Byte_Type
  | Array_Type of naasty_type * array_size option

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

(*Channels and tasks identified uniquely*)
type channel_id = int
type task_id = int

type task =
    Task of task_id * channel_id list * naasty_statement

(*
Target is like an infinite register machine?
*)

(*
Maybe channels should be implicit -- in the connections between tasks
*)

type task_graph =
  task list *
  channel_id list

(*
   Task
   Channel
   TaskGraph

*)

type naasty_program = naasty_statement list

(*FIXME pretty printing*)
let string_of_program = ""

(*FIXME what's the metric for cost?
        maybe it should be a vector: memory use, computational cost, ...*)
let analyse_cost (prog : naasty_program) = 0
let parallelise (prog : naasty_program) : naasty_program list = []
let bound_cost (expense_per_unit : int) (prog : naasty_program) : naasty_program list = []


