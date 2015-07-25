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

type param_entry =
  { key : string;
    parameter_desc : string;
    action : unit -> unit;
    desc : string
  }
;;

let lookup_param (key : string) (param_entries : param_entry list) : param_entry option =
  match List.filter (fun entry -> entry.key = key) param_entries with
  | [] -> None
  | [entry] -> Some entry
  | _ -> failwith ("lookup_param found multiple entries for key " ^ key)
;;

let next_arg : arg_params option ref = ref None in
let arg_idx = ref 1 in

let rec param_table : param_entry list =
  [ { key = "--max_task_cost";
      parameter_desc = "TODO";
      action = (fun () -> failwith "Unsupported feature" (*TODO*));
      desc = "TODO";};
    { key = "--cost_function_file";
      parameter_desc = "TODO";
      action = (fun () -> failwith "Unsupported feature" (*TODO*));
      desc = "TODO";};
    { key = "--disable_inlining";
      parameter_desc = "TODO";
      action = (fun () ->
        cfg := { !cfg with disable_inlining = true });
      desc = "TODO";};
    { key = "--disable_var_erasure";
      parameter_desc = "TODO";
      action = (fun () ->
        cfg := { !cfg with disable_var_erasure = true });
      desc = "TODO";};
    { key = "--debug_output";
      parameter_desc = "TODO";
      action = (fun () ->
        cfg := { !cfg with debug = true });
      desc = "TODO";};
    { key = "-q";
      parameter_desc = "TODO";
      action = (fun () ->
        cfg := { !cfg with output_location = No_output });
      desc = "TODO";};
    { key = "--unexceptional";
      parameter_desc = "TODO";
      action = (fun () ->
        cfg := { !cfg with unexceptional = true });
      desc = "TODO";};
    { key = "-o";
      parameter_desc = "TODO";
      action = (fun () ->
        next_arg := Some OutputDir);
      desc = "TODO";};
    { key = "-I";
      parameter_desc = "TODO";
      action = (fun () ->
        next_arg := Some IncludeDir);
      desc = "TODO";};
    { key = "--infer_type";
      parameter_desc = "TODO";
      action = (fun () ->
        next_arg := Some TypeInfer);
      desc = "TODO";};
    { key =  "--parser_test_file";
      parameter_desc = "TODO";
      action = (fun () ->
        next_arg := Some TestParseFile);
      desc = "TODO";};
    { key = "--parser_test_dir";
      parameter_desc = "TODO";
      action = (fun () ->
        next_arg := Some TestParseDir);
      desc = "TODO";};
    { key = "--runscript";
      parameter_desc = "TODO";
      action = (fun () ->
        cfg := { !cfg with run_compiled_runtime_script = true });
      desc = "TODO";};
    { key = "--no_type_check";
      parameter_desc = "TODO";
      action = (fun () ->
        cfg := { !cfg with skip_type_check = true });
      desc = "TODO";};
    { key = "--version";
      parameter_desc = "TODO";
      action = (fun () ->
        begin
        print_endline ("Otto Flick compiler version " ^ Config.version ^
                       "\nvisit naas-project.org to find out more.");
        exit 0
        end);
      desc = "TODO";};
    { key = "-h";
      parameter_desc = "";
      action = (fun () ->
        begin
          print_endline "Parameters:";
          List.iter (fun entry ->
            print_endline ("  " ^ entry.key ^ " " ^ entry.parameter_desc);
            print_endline ("    " ^ entry.desc);
          ) param_table;
        exit 0
        end);
      desc = "TODO";};
  ] in

while !arg_idx < Array.length Sys.argv do
  let handle_arg idx =
    let s = Sys.argv.(idx) in
    match lookup_param s param_table with
    | Some entry ->
        if !next_arg <> None then
          failwith ("Was expecting a parameter value before " ^ s);
        entry.action ();
    | None ->
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
  if !cfg.run_compiled_runtime_script then
    begin
    Dynlink.init ();
    Runtime_inspect.run []; (*This line does nothing, but ensures that Runtime_inspect
                              is a dependency of Otto. Runtime_inspect will be needed
                              by the loaded script.*)
    try
      Dynlink.loadfile source_file
    with Dynlink.Error error ->
      print_endline (Dynlink.error_message error);
      exit 1
    end
  else
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
