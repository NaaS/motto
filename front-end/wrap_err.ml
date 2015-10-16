(*
   Provides wrapper functions with standard handlers for the module-specific
     exception types.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open Config;;

(*avoids needing OCAMLRUNPARAM="b" in environment at runtime*)
Printexc.record_backtrace true;;

(*FIXME since i only use a single printer for each exception, makes more sense
        to use Printexc.register_printer*)
let wrap (f : 'a -> 'b) (x : 'a) : 'b =
  try f x with
  | Type_infer.Type_Inference_Exc (msg, e, st) ->
    begin
    if not !cfg.unexceptional then
      print_endline
       ("Type error: " ^ msg ^ "\n" ^
(*FIXME        "in file " ^ source_file ^ "\n" ^*)
        "at expression:" ^ Crisp_syntax.expression_to_string
                             Crisp_syntax.min_indentation e ^ "\n" ^
        "state :\n" ^
         State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types)
           true st)
    end;
    exit 1
  | Eval.Eval_Exc (msg, e_opt, rtv_opt) ->
    begin
    if not !cfg.unexceptional then
      let e_s =
        match e_opt with
        | None -> ""
        | Some e ->
          "at expression:" ^ Crisp_syntax.expression_to_string
                               Crisp_syntax.min_indentation e ^ "\n" in
      let rtv_s =
        match rtv_opt with
        | None -> ""
        | Some v ->
          "involving runtime value:" ^ Runtime_data.string_of_typed_value v ^ "\n" in
      print_endline
       ("Evaluation error: " ^ msg ^ "\n" ^
(*FIXME        "in file " ^ source_file ^ "\n" ^*)
        e_s ^
        rtv_s)
    end;
    exit 1
  | Translation.Translation_Expr_Exc (msg, e_opt, local_name_map_opt, sts_acc_opt, st) ->
    begin
    if not !cfg.unexceptional then
      let e_s =
        match e_opt with
        | None -> ""
        | Some e ->
          "at expression:" ^ Crisp_syntax.expression_to_string
                               Crisp_syntax.min_indentation e ^ "\n" in
      let local_name_map_s =
        match local_name_map_opt with
        | None -> ""
        | Some lnm ->
          List.map (fun (l1, l2) -> l1 ^ " |-> " ^ l2) lnm
          |> Debug.print_list "  " in
      let sts_acc_s =
        match sts_acc_opt with
        | None -> ""
        | Some sts_acc ->
          "having so far translated:" ^
            Naasty_aux.string_of_naasty_statement ~st_opt:(Some st)
            2 sts_acc in
      print_endline
       ("Translation error: " ^ msg ^ "\n" ^
(*FIXME        "in file " ^ source_file ^ "\n" ^*)
        e_s ^
        "local_name_map : " ^ local_name_map_s ^ "\n" ^
        sts_acc_s ^ "\n" ^
        "state :\n" ^
        State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types)
           true st)
    end;
    exit 1
  | Inliner.Inliner_Exc (msg, st_opt, stmt_opt) ->
    begin
    if not !cfg.unexceptional then
      let st_s =
        match st_opt with
        | None -> "(no state info)"
        | Some st ->
          "state :\n" ^
          State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types)
           true st in
      let stmt_s =
        match stmt_opt with
        | None -> ""
        | Some stmt ->
          "at statement:" ^ Naasty_aux.string_of_naasty_statement ~st_opt Naasty_aux.no_indent stmt ^ "\n" in
      print_endline
       ("Inlining error: " ^ msg ^ "\n" ^
(*FIXME        "in file " ^ source_file ^ "\n" ^*)
        stmt_s ^
        st_s)
    end;
    exit 1
  | Functions.Functions_Exc (msg, fn, e_opt) ->
    begin
    if not !cfg.unexceptional then
      let e_s =
        match e_opt with
        | None -> ""
        | Some e ->
          "at expression:" ^ Crisp_syntax.expression_to_string
                               Crisp_syntax.min_indentation e ^ "\n" in
      print_endline
       ("Function-lookup error for '" ^ fn ^ "': " ^ msg ^ "\n" ^
(*FIXME        "in file " ^ source_file ^ "\n" ^*)
        e_s)
    end;
    exit 1

  | State.State_Exc (msg, st_opt) ->
    begin
    if not !cfg.unexceptional then
      let st_s =
        match st_opt with
        | None -> ""
        | Some st ->
          "state :\n" ^
            State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types)
             true st in
      print_endline
       ("State consistency error: " ^ msg ^ "\n" ^
(*FIXME        "in file " ^ source_file ^ "\n" ^*)
        st_s)
    end;
    exit 1

  | Naasty_aux.Naasty_aux_Exc (msg, st_opt) ->
    begin
    if not !cfg.unexceptional then
      let st_s =
        match st_opt with
        | None -> ""
        | Some st ->
          "state :\n" ^
            State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types)
             true st in
      print_endline
       ("Intermedate Language error: " ^ msg ^ "\n" ^
(*FIXME        "in file " ^ source_file ^ "\n" ^*)
        st_s)
    end;
    exit 1

  | e ->
    if !cfg.unexceptional then exit 1
    else raise e
