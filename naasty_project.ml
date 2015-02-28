(*
   Model of the organisation of NaaSty code into different files of different
   kinds.
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open Naasty

(*Types of compilation units*)
type unit_type =
  | Header
  | Cpp

type compilation_unit =
  { name : string;
    unit_type : unit_type;
    (*NOTE overlap of "inclusions" with state.inclusions*)
    inclusions : string list;
    content : naasty_program }

let filename_of_compilationunit (cu : compilation_unit) : string =
  match cu.unit_type with
  | Header -> cu.name ^ ".h"
  | Cpp -> cu.name ^ ".cpp"

type naasty_project = compilation_unit list
