(*
   Interface for compiler backend.
   Nik Sultana, Cambridge University Computer Lab, June 2015
*)

module type Instance =
sig
  (*Unit name could be a filename*)
  type unit_name = string
  (*Contents of a unit*)
  type unit_contents = string
  (*Translate to a list of compilation units*)
  val translate : State.state ->
                   (Crisp_project.compilation_unit *
                    Crisp_project.compilation_unit *
                    Crisp_project.compilation_unit) ->
    (unit_name * unit_contents) list
end
