(*
   Interactive runtime for Flick programs.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open State
open Crisp_syntax

type inspect_instruction =
    (*declare and define variable, and initialise*)
  | Declare_value of string * string
    (*set - variable value*)
  | Set of string * string
    (*channel - declare and define channel (in the symbol table and runtime context)*)
  | Declare_channel of string
    (*close_channel - break a channel (the connected processes should react to this)*)
  | Close_channel of string
    (*queue channel value*)
  | Q_channel of string * string
    (*dequeue channel value*)
  | Deq_channel of string
    (*load - Flick program from file*)
  | Load of string
    (*Evaluate a Flick expression.
      If a process is called, then it runs for a single iteration.*)
  | Eval of string
    (*execute some meta-instruction, e.g., to show the whole runtime context,
      or specific parts of it, or the symbol_table*)
  | MI of meta_instruction

(*Evaluate a single inspect-instruction*)
let eval (st : state) (ctxt : Runtime_data.runtime_ctxt) (i : inspect_instruction) : Runtime_data.runtime_ctxt =
  failwith "TODO"

(*Evaluate a list of inspect-instructions*)
let evals (st : state) (ctxt : Runtime_data.runtime_ctxt) (is : inspect_instruction list) : Runtime_data.runtime_ctxt =
  List.fold_right (fun instr ctxt ->
    eval st ctxt instr) is ctxt
