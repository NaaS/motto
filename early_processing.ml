(*
   High-level transformations applied to the source program.
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open State
open Crisp_syntax
open Naasty
open Data_model
open Type_infer (*FIXME currently unused*)

let expand_includes (include_directories : string list) (p : Crisp_syntax.program) =
  let rec expand_includes' (p : Crisp_syntax.program) =
    List.fold_right (fun decl acc ->
      match decl with
      | Include source_file ->
        (*FIXME take include_directories into account; currently this info is
                unused.*)
        (*FIXME when all the include directories have been exhausted and the
                file hasn't been found yet, then try the current directory.*)
        let inclusion =
          Crisp_parse.parse_file source_file
          |> (fun p -> match p with
               | Program p -> p
               | _ -> failwith "Inclusion file does not contain a program")
          |> List.rev
        in expand_includes' inclusion @ acc
      | _ -> decl :: acc) p []
  in List.rev (expand_includes' p)

(*Gather declaration information from a program, and encode in the state.*)
let collect_decl_info (st : State.state) (p : Crisp_syntax.program) : State.state =
  let fun_decls, ty_decls, proc_decls, st' =
  List.fold_right (fun decl (acc_fun_decls, acc_ty_decls, acc_proc_decls, st) ->
    match decl with
    | Function {fn_name; fn_params; _} ->
      ((fn_name, fn_params) :: acc_fun_decls, acc_ty_decls, acc_proc_decls, st)
    | Type {type_name; type_value} ->
      let nst_ty, st' = Translation.naasty_of_flick_type st type_value in
      (acc_fun_decls, (type_name, type_value, nst_ty) :: acc_ty_decls, acc_proc_decls, st')
    | Process _ ->
      (*FIXME currently we ignore process declarations*)
      (acc_fun_decls, acc_ty_decls, acc_proc_decls, st)
    | Include _ ->
      failwith "Inclusions should have been expanded before reaching this point.")
    p ([], [], [], st) in
  { st' with crisp_funs =
               (*NOTE order of declarations is preserved*)
               st'.crisp_funs @ List.rev fun_decls;
             type_declarations = st'.type_declarations @ List.rev ty_decls}

(*Given a program whose Includes have been expanded out, separate out the
  declarations of types, processes, and functions -- but keep their relative
  order stable. That is, if we rewrite the program to contain the types, then
  the functions, then the processes, then all existing dependencies should
  continue to be satisified. (Originally, types, functions and processes can be
  given in any order as long as usual scoping rules are satisfied.)
  Then chop source file into different units, collecting declarations of the
  same kind*)
let split_declaration_kinds (p : Crisp_syntax.program) :
  Crisp_project.compilation_unit * Crisp_project.compilation_unit *
  Crisp_project.compilation_unit =
  List.fold_right (fun decl (types, functions, processes) ->
    match decl with
    | Type _ -> (decl :: types, functions, processes)
    | Process _ -> (types, functions, decl :: processes)
    | Function _ -> (types, decl :: functions, processes)
    | Include _ ->
      failwith "Inclusions should have been expanded before reaching this point.")
    p ([], [], [])
  |> (fun (types, functions, processes) ->
    ({ Crisp_project.name = "types";
       Crisp_project.content = List.rev types },
     { Crisp_project.name = "functions";
       Crisp_project.content = List.rev functions },
     { Crisp_project.name = "processes";
       Crisp_project.content = List.rev processes }))

(*Every type becomes 2 compilation units in NaaSty: a header file and a code
  file.*)
let translate_type_compilation_unit (st : state)
      (types_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_couple ([], st)
    (fun (st' : state) (decl : Crisp_syntax.toplevel_decl)
      (cunits : Naasty_project.compilation_unit list) ->
       let name = Crisp_syntax_aux.name_of_type decl in
       let (translated, st'') =
         Translation.naasty_of_flick_program ~st:st' [decl] in
       let module Data_model_instance =
         Instance(Data_model_consts.Values(
         struct
           let datatype_name = name
           let ty = Crisp_syntax_aux.the_ty_of_decl decl
         end)) in
       let (type_data_model_instance, st''') =
         fold_map ([], st'') (fun st scheme ->
           Naasty_aux.instantiate_type true scheme.identifiers st scheme.type_scheme)
           Data_model_instance.instantiate_data_model in
       let header_unit =
         {Naasty_project.name = name;
          Naasty_project.unit_type = Naasty_project.Header;
          (*FIXME currently hardcoded, but this list of inclusions could be
                  extended based on an analysis of the code in the module.*)
          Naasty_project.inclusions =
            ["<stdint.h>";
             "<iostream>";
             "<assert.h>";
             "<exception>";
             "\"TaskBuffer.h\"";
             "\"applications/NaasData.h\""];
          Naasty_project.content =
            [Naasty_aux.add_fields_to_record (the_single translated)
               type_data_model_instance]
         } in
       let (function_data_model_instance, st4) =
         fold_map ([], st''') (fun st scheme ->
           Naasty_aux.instantiate_function true scheme.identifiers st
             scheme.function_scheme)
           Data_model_instance.instantiate_data_model in
       let cpp_unit =
         {Naasty_project.name = name;
          Naasty_project.unit_type = Naasty_project.Cpp;
          (*FIXME currently hardcoded, but this list of inclusions could be
                  extended based on an analysis of the code in the module.*)
          Naasty_project.inclusions =
            ["\"" ^ Naasty_project.filename_of_compilationunit header_unit ^ "\"";
             "\"LinearBuffer.h\"";
             "<iostream>";
             "\"utils/ReadWriteData.h\"";
             "\"applications/NaasData.h\""];
          Naasty_project.content =
            List.map (fun fn -> Fun_Decl fn) function_data_model_instance
         }
       in (header_unit :: cpp_unit :: cunits, st4))
    types_unit.Crisp_project.content

let translate_function_compilation_unit (st : state)
      (functions_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_map ([], st)
    (fun (st' : state) (flick_f : Crisp_syntax.toplevel_decl) ->
       let name = "functions"(*FIXME extract name?*) in
       let (translated, st'') =
         Translation.naasty_of_flick_program ~st:st' [flick_f] in
       ({Naasty_project.name = name;
         Naasty_project.unit_type = Naasty_project.Cpp;
         Naasty_project.inclusions =
           [(*FIXME*)];
         Naasty_project.content = translated
        }, (*FIXME generate header file together with this .cpp*)
        st''))
    functions_unit.Crisp_project.content

(*FIXME currently ignoring processes*)
let translate_serialise_stringify
  (st : State.state)
  ((types_unit, functions_unit, processes_unit) :
     Crisp_project.compilation_unit *
     Crisp_project.compilation_unit *
     Crisp_project.compilation_unit) =
  let stringify_compilation_unit (st : state) (cu : Naasty_project.compilation_unit) =
    (Naasty_project.filename_of_compilationunit cu,
     Naasty_project.string_of_compilationunit ~st_opt:(Some st) cu) in
  let (translated_type_units, st') =
    translate_type_compilation_unit st types_unit in
  let (translated_function_units, st'') =
    translate_function_compilation_unit st' functions_unit in
  if !Config.cfg.Config.debug then State_aux.state_to_str true st'' |> print_endline;
  List.map (stringify_compilation_unit st'')
    (translated_type_units @ translated_function_units)

(*Check that have distinct parameter names, otherwise using named
  parameters could be confusing. Also checks that all parameters
  have names (otherwise we won't be able to refer to them in the function
  body) -- though this should already have been checked by the parser.*)
let check_distinct_parameter_names (st : state) : state =
  let _ =
    begin
    List.iter (fun (function_name, function_type) ->
      let ((chans, arg_tys), ret_tys) =
        Crisp_syntax_aux.extract_function_types function_type in
      assert (chans = []); (*FIXME currently functions cannot be given channel
                             parameters*)
      ignore (List.fold_right (fun ty acc ->
        match Crisp_syntax_aux.label_of_type ty with
        | None -> failwith "All function parameters must be named"
        | Some label ->
          if List.exists (fun lbl -> lbl = label) acc then
            (*This is the problem that this analysis is designed to catch.*)
            failwith ("Function " ^ function_name ^ " has two parameters called " ^ label)
          else label :: acc) arg_tys [])) st.crisp_funs
    end
  in st

let compile (cfg : Config.configuration ref) (program : Crisp_syntax.program) : (string * string) list =
  expand_includes !cfg.Config.include_directories program
  |> selfpair
  |> apfst (collect_decl_info initial_state)
  |> apfst check_distinct_parameter_names
  |> apsnd split_declaration_kinds
  (*FIXME Functorise to take backend-specific code as parameter*)
  |> uncurry translate_serialise_stringify
