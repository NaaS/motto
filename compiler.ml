(*
   High-level spec of the compiler.
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open State
open Crisp_syntax
open Naasty
open Type_infer

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
      let st' =
        match lookup_term_data (Term Function_Name) st.term_symbols fn_name with
        | None ->
          let (fn_idx, st') =
            if Naasty_aux.is_fresh fn_name st then
              Naasty_aux.extend_scope_unsafe (Term Function_Name) st ~ty_opt:None(*FIXME put function's type here?*)
                                fn_name
            else
              failwith ("Function name " ^ fn_name ^ " isn't fresh.") in
          (*NOTE order of declarations isn't preserved within term_symbols*)
          st'
        | Some (_, _) -> failwith ("Function " ^ fn_name ^ " declared more than once") in
      ((fn_name, fn_params) :: acc_fun_decls, acc_ty_decls, acc_proc_decls, st')
    | Type {type_name; type_value} ->
      let st' =
        match Crisp_syntax_aux.consts_in_type type_value with
        | None -> st
        | Some consts ->
          List.fold_right (fun (name, ik, ty) st ->
            let (_, st') =
              if Naasty_aux.is_fresh name st then
                Naasty_aux.extend_scope_unsafe (Term ik) st ~src_ty_opt:(Some ty) ~ty_opt:None name
              else
                failwith ("Constant " ^ name ^ " isn't fresh.") in
            (*NOTE order of declarations isn't preserved within term_symbols*)
            st') consts st in
      let nst_ty, st'' =
        if !Config.cfg.Config.translate then
          let nst_ty, st'' =
            Translation.naasty_of_flick_type ~default_label:(Some type_name)
             st' type_value in
          (Some nst_ty, st'')
        else (None, st') in
      (acc_fun_decls, (type_name, type_value, nst_ty) :: acc_ty_decls, acc_proc_decls, st'')
    | Process _ ->
      (*FIXME currently we ignore process declarations*)
      (acc_fun_decls, acc_ty_decls, acc_proc_decls, st)
    | Include _ ->
      failwith "Inclusions should have been expanded before reaching this point.")
    p ([], [], [], st) in
  (*NOTE order of declarations is preserved*)
  { st' with crisp_funs = st'.crisp_funs @ List.rev fun_decls;
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
  |> (fun ((st, p) as data) ->
    (if !Config.cfg.Config.debug then
       State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types) true st
       |> print_endline;
       data))
  (*FIXME Functorise to take backend-specific code as parameter*)
  |> (fun x ->
        if !Config.cfg.Config.translate then
          uncurry Icl_backend.translate x
        else [])
