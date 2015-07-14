(*
   Provides wrapper functions with standard handlers for the module-specific
     exception types.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open Config

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
  | e ->
    if !cfg.unexceptional then exit 1
    else raise e
