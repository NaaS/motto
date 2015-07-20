(*
   Evaluation monad
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)


open Crisp_syntax
open State
open Runtime_data

type eval_continuation = state -> runtime_ctxt -> eval_m * runtime_ctxt
(*FIXME update 'normalise' to return values of this type*)
and eval_m =
    (*Expression will not be normalised any further*)
  | Value of expression
    (*Expression should be normalised further.
      If it reduces to a value in one step, then continue immediately to
      the first function parameter. Otherwise, use the second function
      parameter to combine any background continuation with this continuation,
      to create a new (combined) continuation.*)
  | Cont of expression * (expression -> eval_continuation) *
    ((expression -> eval_continuation) -> (expression -> eval_continuation))

(*FIXME bad style*)
let expect_value m =
  match m with
  | Value e -> e

let return_eval (e : expression) : eval_m = Value e
let continuate f e = Cont (e, f, (fun x -> x))

let rec bind_eval normalise (e : expression) (f : expression -> eval_continuation) st ctxt : eval_m * runtime_ctxt =
  let em, ctxt' = normalise st ctxt e in
  match em with
  | Value e' -> f e' st ctxt'
  | Cont (e', _, compose) ->
    Cont (e', compose f, (fun x -> x)), ctxt'

and run normalise st ctxt (work_list : eval_m list) : expression list * eval_m list * runtime_ctxt =
  List.fold_right (fun m (el, wl, ctxt) ->
    match m with
    | Value e ->
      (*The value won't be fed into further continuations, so it's removed from
        the work list and added to the result list*)
      (e :: el, wl, ctxt)
    | Cont (e, f, _) ->
      let em, ctxt' = bind_eval normalise e f st ctxt in
      (el, em :: wl, ctxt')) work_list ([], [], ctxt)

and run_until_done normalise st ctxt work_list results =
  if work_list = [] then results, ctxt
  else
    let el, wl, ctxt' = run normalise st ctxt work_list in
    run_until_done normalise st ctxt' wl (el @ results)
