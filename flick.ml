(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

type configuration =
  { source_file : string option;
    output_directory : string option;
    max_task_cost : int option;
    cost_function_file : string option;
    (*Include directories are ordered by priority in which they are searched;
      this is in the reverse order they are provided on the command line.
      i.e., -I searched_dir_2 -I searched_dir_1*)
    include_directories : string list;
  }
type arg_params =
  | OutputDir
  | IncludeDir
;;

let cfg : configuration ref = ref {
  source_file = None;
  output_directory = None;
  max_task_cost = None;
  cost_function_file = None;
  include_directories = [];
} in
let next_arg : arg_params option ref = ref None in
let arg_idx = ref 1 in

while !arg_idx < Array.length Sys.argv do
  let handle_arg idx =
    match Sys.argv.(idx) with
    | "--max_task_cost" -> failwith "Unsupported feature" (*TODO*)
    | "--cost_function_file" -> failwith "Unsupported feature" (*TODO*)
    | "-o" ->
      if !next_arg <> None then
        failwith ("Was expecting a parameter value before " ^ Sys.argv.(idx))
      else next_arg := Some OutputDir
    | "-I" ->
      if !next_arg <> None then
        failwith ("Was expecting a parameter value before " ^ Sys.argv.(idx))
      else next_arg := Some IncludeDir
    | s ->
      match !next_arg with
      | None ->
        cfg := { !cfg with source_file = Some s }
      | Some OutputDir ->
        cfg := { !cfg with output_directory = Some s };
        next_arg := None
      | Some IncludeDir ->
        cfg := { !cfg with include_directories = s :: !cfg.include_directories};
        next_arg := None
  in
  handle_arg !arg_idx;
  arg_idx := !arg_idx + 1
done;

match !cfg.source_file, !cfg.output_directory with
| Some source_file, Some output_directory ->
  Crisp_parse.parse source_file
  |> Serialisation.expand_includes !cfg.include_directories
  |> Serialisation.split_declaration_kinds
  |> Serialisation.translate_serialise_stringify State.initial_state
  |> General.write_files
| _ ->
  begin
    failwith "Output directory and input file need to be specified";
  end
