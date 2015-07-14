(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Config

type arg_params =
  | OutputDir
  | IncludeDir
  | TypeInfer
  | TestParseFile
  | TestParseDir
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
    | "-q" ->
      cfg := { !cfg with output_location = No_output }
    | "--unexceptional" ->
      cfg := { !cfg with unexceptional = true }
    | "-o" ->
      if !next_arg <> None then
        failwith ("Was expecting a parameter value before " ^ Sys.argv.(idx))
      else next_arg := Some OutputDir
    | "-I" ->
      if !next_arg <> None then
        failwith ("Was expecting a parameter value before " ^ Sys.argv.(idx))
      else next_arg := Some IncludeDir
    | "--infer_type" ->
      if !next_arg <> None then
        failwith ("Was expecting a parameter value before " ^ Sys.argv.(idx))
      else next_arg := Some TypeInfer
    | "--parser_test_file" ->
      if !next_arg <> None then
        failwith ("Was expecting a parameter value before " ^ Sys.argv.(idx))
      else next_arg := Some TestParseFile
    | "--parser_test_dir" ->
      if !next_arg <> None then
        failwith ("Was expecting a parameter value before " ^ Sys.argv.(idx))
      else next_arg := Some TestParseDir
    | "--no_type_check" ->
      cfg := { !cfg with skip_type_check = true }
    | "--version" ->
      print_endline ("Otto Flick compiler version " ^ Config.version ^
                     "\nvisit naas-project.org to find out more.");
      exit 0
    | s ->
      match !next_arg with
      | None ->
        begin
        match (!cfg).source_file with
        | None ->
          cfg := { !cfg with source_file = Some s }
        | Some remaining_params ->
          failwith ("Parameters seem incorrect. Cannot handle: " ^ remaining_params)
        end
      | Some OutputDir ->
        cfg := { !cfg with output_location = Directory s };
        next_arg := None
      | Some IncludeDir ->
        cfg := { !cfg with include_directories = s :: !cfg.include_directories};
        next_arg := None
      | Some TypeInfer ->
        let e =
          match Crisp_parse.parse_string s with
          | Crisp_syntax.Expression e -> e
          | _ -> failwith "String is not an expression" in
        let ty, ty_env =
          Type_infer.ty_of_expr ~strict:true State.initial_state e in
        let ty_s =
          Crisp_syntax.type_value_to_string true false Crisp_syntax.min_indentation ty in
        Printf.printf "%s" ty_s;
        next_arg := None
      | Some TestParseFile ->
        cfg := { !cfg with parser_test_files = s :: !cfg.parser_test_files};
        next_arg := None
      | Some TestParseDir ->
        cfg := { !cfg with parser_test_dirs = s :: !cfg.parser_test_dirs};
        next_arg := None
  in
  handle_arg !arg_idx;
  arg_idx := !arg_idx + 1
done;

match !cfg.source_file with
| Some source_file ->
  let compile file =
    Compiler.parse_program file
    |> Compiler.front_end cfg
    |> Compiler.back_end cfg
    |> Output.write_files !cfg.output_location in
  Wrap_err.wrap compile source_file
| _ ->
  if !cfg.parser_test_files <> [] || !cfg.parser_test_dirs <> [] then
    Crisp_test.run_parser_test !cfg.parser_test_dirs !cfg.parser_test_files;
  if !cfg.output_location <> No_output then
    Printf.printf "(Not given a file to compile)"
