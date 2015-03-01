(*
   Model of the organisation of NaaSty code into different files of different
   kinds.
   Nik Sultana, Cambridge University Computer Lab, February 2015

   NOTE currently we don't check to see if compilation unit filenames are unique
        -- i.e., we could have two files called "things.h".
*)

(*Types of compilation units*)
type unit_type =
  | Header
  | Cpp

type compilation_unit =
  { name : string;
    unit_type : unit_type;
    (*NOTE overlap of "inclusions" with state.inclusions*)
    (*NOTE that we expect inclusions to be quoted suitably -- e.g., some in
           quote marks (e.g., "TaskBuffer.h" and others in angled brackets (e.g.,
           <stdint.h>), as appropriate.*)
    (*NOTE that we expect inclusions to be provided in the order that they'll be
           added to the file -- that kind of analysis/reordering takes place
           separately from turning a compilation-unit record into a string.*)
    inclusions : string list;
    content : Naasty.naasty_program }

(*Maps a compilation unit into the name of the file that will contain it when
  stored on the file system.*)
let filename_of_compilationunit (cu : compilation_unit) : string =
  match cu.unit_type with
  | Header -> cu.name ^ ".h"
  | Cpp -> cu.name ^ ".cpp"

(*Turns a compilation unit into the textual content it will contain.*)
let string_of_compilationunit ?st_opt:((st_opt : State.state option) = None)
      (cu : compilation_unit) : string =
  let inclusions =
    List.map (fun s -> "#include " ^ s ^ "") cu.inclusions
    |> String.concat "\n" in
  let prefix, suffix =
    match cu.unit_type with
    | Header ->
      let def_name =
        (*FIXME we don't check if def-names are unique*)
        "__" ^ String.uppercase cu.name ^ "_H_"
      in ("#ifndef " ^ def_name ^ "\n#define " ^ def_name ^ "\n", "#endif")
    | Cpp -> ("", "") in
  let body =
    Naasty_aux.string_of_naasty_program ~st_opt Naasty_aux.prog_indentation
      cu.content
  in prefix ^ body ^ suffix

type project = compilation_unit list
