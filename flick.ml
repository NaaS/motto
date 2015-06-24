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
      | Some TypeInfer ->
        let e =
          match Crisp_parse.parse_string s with
          | Crisp_syntax.Expression e -> e
          | _ -> failwith "String is not an expression" in
        let ty, ty_env =
          Type_infer.ty_of_expr ~strict:true [] e in
        let ty_s =
          Crisp_syntax.type_value_to_string true false Crisp_syntax.min_indentation ty in
        Printf.printf "%s" ty_s
  in
  handle_arg !arg_idx;
  arg_idx := !arg_idx + 1
done;

match !cfg.source_file with
| Some source_file ->
  Crisp_parse.parse_file source_file
  |> (fun p -> match p with
       | Crisp_syntax.Program p -> p
       | _ -> failwith "Source file does not contain a program")
  |> Early_processing.compile cfg
  |> Output.write_files !cfg.output_location
| _ ->
  if !cfg.output_location <> No_output then
    Printf.printf "(No input file specified)"
