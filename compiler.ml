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

(*blob here is either function or process, as long as they both return a single
  thing. in the case of process it doesn't matter, since nothing is ever returned*)
let type_check_blob (st : State.state) (chans : channel list)
      (args : type_value list) (ret : type_value) (pb : process_body) : (bool * (type_value * type_value)) =
  let st' =
    List.fold_right (fun ((Channel (ct, name)) : channel) (st : state) ->
        let scope = Term Channel_Name in
        Naasty_aux.extend_scope_unsafe scope st
          ~src_ty_opt:(Some (ChanType (Some name, ct))) name
        |> snd) chans st
    |> List.fold_right (fun (arg : type_value) (st : state) ->
        let scope = Term Value(*FIXME*) in
        let label =
          match Crisp_syntax_aux.label_of_type arg with
          | None -> failwith "Came across anonymous parameter"(*FIXME give more info*)
          | Some lbl -> lbl in
        Naasty_aux.extend_scope_unsafe scope st ~src_ty_opt:(Some arg)
          label
        |> snd) args in

  (*FIXME currently ignoring state and exceptions.
          exceptions are straightforward to type check: just add a name
            to the typing context, and check the exception body.
          state is less straightforward: i think i should hoist its info
            globally (where it morally belongs) and perhaps classify variables
            using a scheme like that below, to ensure that variables aren't
            used in the wrong context (but also that, if shared, variables can
            be used in different functions):

            type variable_scope =
              | Local
              | Global
            type variable_affinity =
              | Unique
              | Shared*)
  let (st_decls, e, ex_decls) = Crisp_syntax_aux.extract_process_body_bits pb in

  let actual_ret =
    ty_of_expr ~strict:true st' e
    |> fst
    |> Crisp_syntax_aux.forget_label in
  let ret = Crisp_syntax_aux.forget_label ret in
  (Crisp_syntax_aux.type_match ret actual_ret, (ret, actual_ret))

(*Gather declaration information from a program, and encode in the state.*)
let collect_decl_info (st : State.state) (p : Crisp_syntax.program) : State.state =
  List.fold_right (fun decl st ->
    match decl with
    | Function {fn_name; fn_params; fn_body} ->
      begin
      let st =
        { st with crisp_funs = (fn_name, (true, fn_params)) :: st.crisp_funs} in
      match lookup_term_data (Term Function_Name) st.term_symbols fn_name with
      | None ->
        let (fn_idx, st') =
          if Naasty_aux.is_fresh fn_name st then
            Naasty_aux.extend_scope_unsafe (Term Function_Name) st ~ty_opt:None(*FIXME put function's type here?*)
                              fn_name
          else
            failwith ("Function name " ^ fn_name ^ " isn't fresh.") in

        let _ =
          let ((chans, arg_tys), ret_tys) =
            Crisp_syntax_aux.extract_function_types fn_params in
          let ret_ty =
            match ret_tys with
            | [] -> flick_unit_type
            | [ty] -> ty
            | _ -> failwith "Multifunctions not supported"(*FIXME give more info*) in
          if !Config.cfg.Config.skip_type_check then ()
          else
            match type_check_blob st chans arg_tys ret_ty fn_body with
            | (true, _) -> ()
            | (false, (expected_ty, actual_ty)) ->
              let expected_ty_s = type_value_to_string true false min_indentation expected_ty in
              let actual_ty_s = type_value_to_string true false min_indentation actual_ty in
              failwith ("Types don't check in " ^ fn_name ^ " expected: " ^ expected_ty_s ^ " but found " ^ actual_ty_s) in
        (*NOTE order of declarations isn't preserved within term_symbols*)
        st'
      | Some (_, _) -> failwith ("Function " ^ fn_name ^ " declared more than once")
      end
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
      { st'' with type_declarations =
                    (type_name, type_value, nst_ty) :: st''.type_declarations}
    | Process {process_name; process_type; process_body} ->
      (*FIXME work in progress:
              some duplication with code that handles function declarations.
              seems like a good idea to kinda treat this as a function*)
      let fn_params =
        match process_type with
        | ProcessType (_, (chans, args)) ->
          FunType (FunDomType (chans, args), FunRetType [flick_unit_type]) in
      begin
      let st =
        { st with crisp_funs = (process_name, (false, fn_params)) :: st.crisp_funs} in
      match lookup_term_data (Term Function_Name) st.term_symbols process_name with
      | None ->
        let (fn_idx, st') =
          if Naasty_aux.is_fresh process_name st then
            Naasty_aux.extend_scope_unsafe (Term Function_Name) st
              ~ty_opt:None(*FIXME put process's type here?*)
                              process_name
          else
            failwith ("Process name " ^ process_name ^ " isn't fresh.") in

        let _ =
          let ((chans, arg_tys), ret_tys) =
            Crisp_syntax_aux.extract_function_types fn_params in

          let ret_ty = flick_unit_type in

          if !Config.cfg.Config.skip_type_check then ()
          else
            match type_check_blob st chans arg_tys ret_ty process_body with
            | (true, _) -> ()
            | (false, (expected_ty, actual_ty)) ->
              let expected_ty_s = type_value_to_string true false min_indentation expected_ty in
              let actual_ty_s = type_value_to_string true false min_indentation actual_ty in
              failwith ("Types don't check in " ^ process_name ^ " expected: " ^ expected_ty_s ^ " but found " ^ actual_ty_s) in
        (*NOTE order of declarations isn't preserved within term_symbols*)
        st'
      | Some (_, _) -> failwith ("Process " ^ process_name ^ " declared more than once")
      end


    | Include _ ->
      failwith "Inclusions should have been expanded before reaching this point.")
    p st

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
    List.iter (fun (function_name, (is_fun, function_type)) ->
      let ((chans, arg_tys), ret_tys) =
        Crisp_syntax_aux.extract_function_types function_type in
      let arg_names = List.map Crisp_syntax_aux.label_of_type arg_tys in
      let channel_names =
        List.map (fun cn -> Some (Crisp_syntax_aux.label_of_channel cn)) chans in
      ignore (List.fold_right (fun name_opt acc ->
        let thing = if is_fun then "function" else "process" in
        match name_opt with
        | None -> failwith ("All " ^ thing ^ " parameters must be named")
        | Some label ->
          if List.exists (fun lbl -> lbl = label) acc then
            (*This is the problem that this analysis is designed to catch.*)
            failwith (thing ^ " " ^ function_name ^ " has two parameters called " ^ label)
          else label :: acc) (channel_names @ arg_names) [])) st.crisp_funs
    end
  in st

(*Parse a file and expect to find a program in it*)
let parse_program source_file =
  Crisp_parse.parse_file source_file
  |> (fun p -> match p with
       | Crisp_syntax.Program p -> p
       | _ -> failwith "Source file does not contain a program")

let front_end ?st:(st : state = initial_state) (cfg : Config.configuration ref) (program : Crisp_syntax.program) =
  expand_includes !cfg.Config.include_directories program
  |> selfpair
  |> apfst (collect_decl_info st)
  |> apfst check_distinct_parameter_names
  |> apsnd split_declaration_kinds
  |> (fun ((st, p) as data) ->
    (if !Config.cfg.Config.debug then
       State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types) true st
       |> print_endline;
       data))

(*FIXME Functorise to take backend-specific code as parameter*)
let back_end (cfg : Config.configuration ref) inputs : (string * string) list =
  if !Config.cfg.Config.translate then
    uncurry Icl_backend.translate inputs
  else []
