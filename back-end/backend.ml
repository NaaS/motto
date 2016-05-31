(*
   Interface for compiler backend.
   Nik Sultana, Cambridge University Computer Lab, June 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

type compilation_record =
  {
    types_unit : Crisp_project.compilation_unit;
    functions_unit : Crisp_project.compilation_unit;
    processes_unit : Crisp_project.compilation_unit;
  }

module type Instance =
sig
  (*Unit name could be a filename*)
  type unit_name = string
  (*Contents of a unit*)
  type unit_contents = string
  (*Transform a program before compiling it*)
  val preprocess : State.state -> Crisp_syntax.program ->
    Crisp_syntax.program * State.state
  (*Translate to a list of compilation units*)
  val translate : State.state -> compilation_record ->
    (unit_name * unit_contents) list
end
