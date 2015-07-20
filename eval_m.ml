(*
   Evaluation monad
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)


open Crisp_syntax
open State
open Runtime_data

type eval_continuation = state -> runtime_ctxt -> eval_m * runtime_ctxt
(*FIXME add other kinds of result values -- such as Timeout, and perhaps
        one that carries exceptions*)
and eval_m =
    (*Expression will not be normalised any further*)
  | Value of expression
    (*Expression should be normalised further.
      If it reduces to a value in one step, then continue immediately to
      the first function parameter. Otherwise, use the second function
      parameter to combine any background continuation with this continuation,
      to create a new (combined) continuation.*)
  | Cont of expression * (expression -> eval_continuation)
  | Bind of eval_m * (expression -> eval_continuation)

let rec evalm_to_string : eval_m -> string = function
  | Value e -> "Value " ^ expression_to_string min_indentation e
  | Cont (e, _) -> "Cont (" ^ expression_to_string min_indentation e ^ ", _)"
  | Bind (m, _) -> "Bind (" ^ evalm_to_string m ^ ", _)"

(*FIXME bad style*)
let expect_value m =
  match m with
  | Value e -> e

let return_eval (e : expression) : eval_m = Value e
let return (e : expression) : eval_continuation = fun _ ctxt -> Value e, ctxt
let continuate e f = Cont (e, f)

let rec bind_eval normalise (m : eval_m) (f : expression -> eval_continuation) st ctxt : eval_m * runtime_ctxt =
  match m with
(*
  | Value e' -> f e' st ctxt
  | Bind (Value e', f') ->
    let m', ctxt' = f' e' st ctxt in
    bind_eval normalise m' f st ctxt'
(*    Bind (m', f), ctxt'*)
  | Bind (em, f') ->
    let m', ctxt' = bind_eval normalise em f' st ctxt in
    bind_eval normalise m' f st ctxt'
  | Cont (e, f') ->
    let em, ctxt' = normalise st ctxt e in
    Bind (Bind (em, f'), f), ctxt'
*)
  | Value e' -> f e' st ctxt
(*
  | Bind (Value e', f') ->
    let m', ctxt' = f' e' st ctxt in
    bind_eval normalise m' f st ctxt'
(*    Bind (m', f), ctxt'*)
*)
  | Bind (em, f') ->
    begin
    let m', ctxt' = bind_eval normalise em f' st ctxt in
    match m' with
    | Value e' -> f e' st ctxt'
    | _ -> Bind (m', f), ctxt'
    end
  | Cont (e, f') ->
    let em, ctxt' = normalise st ctxt e in
    Bind (Bind (em, f'), f), ctxt'

and run normalise st ctxt (work_list : eval_m list) : expression list * eval_m list * runtime_ctxt =
  List.fold_right (fun m (el, wl, ctxt) ->
    print_endline (evalm_to_string m);
    match m with
    | Value e ->
      (*The value won't be fed into further continuations, so it's removed from
        the work list and added to the result list*)
      (e :: el, wl, ctxt)
    | _ ->
      let em, ctxt' = bind_eval normalise m return st ctxt in
      (el, em :: wl, ctxt')) work_list ([], [], ctxt)

and run_until_done normalise st ctxt work_list results =
  if work_list = [] then results, ctxt
  else
    let el, wl, ctxt' = run normalise st ctxt work_list in
    run_until_done normalise st ctxt' wl (el @ results)
