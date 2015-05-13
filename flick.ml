(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open Config

type arg_params =
  | OutputDir
  | IncludeDir
;;

let next_arg : arg_params option ref = ref None in
let arg_idx = ref 1 in

while !arg_idx < Array.length Sys.argv do
  let handle_arg idx =
    match Sys.argv.(idx) with
    | "--max_task_cost" -> failwith "Unsupported feature" (*TODO*)
    | "--cost_function_file" -> failwith "Unsupported feature" (*TODO*)
    | "--disable_inlining" ->
      cfg := { !cfg with disable_inlining = true }
    | "--disable_var_erasure" ->
      cfg := { !cfg with disable_var_erasure = true }
    | "--debug_output" ->
      cfg := { !cfg with debug = true }
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
        if (!cfg).source_file <> None then
          failwith "Parameters seem incorrect"
        else
          cfg := { !cfg with source_file = Some s }
      | Some OutputDir ->
        cfg := { !cfg with output_location = Directory s };
        next_arg := None
      | Some IncludeDir ->
        cfg := { !cfg with include_directories = s :: !cfg.include_directories};
        next_arg := None
  in
  handle_arg !arg_idx;
  arg_idx := !arg_idx + 1
done;

match !cfg.source_file with
| Some source_file ->
  Crisp_parse.parse source_file
  |> Serialisation.expand_includes !cfg.include_directories
  |> Serialisation.split_declaration_kinds
  (*FIXME "Serialisation" doesn't seem like most sensible module in which to
           keep this code.*)
  (*FIXME Functorise to take backend-specific code as parameter*)
  |> Serialisation.translate_serialise_stringify State.initial_state
  |> Output.write_files !cfg.output_location
| _ ->
  begin
    failwith "Output directory and input file need to be specified";
  end
