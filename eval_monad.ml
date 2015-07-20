(*
   Evaluation monad
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)


open Crisp_syntax
open State
open Runtime_data

type eval_continuation = state -> runtime_ctxt -> eval_monad * runtime_ctxt

and bind_kind =
  | Fun of (expression -> eval_continuation)
  | Id

(*FIXME add other kinds of result values -- such as Timeout, and perhaps
        one that carries exceptions*)
and eval_monad =
    (*Expression will not be normalised any further*)
  | Value of expression
    (*Expression should be normalised further.
      If it reduces to a value in one step, then continue immediately to
      the first function parameter. Otherwise, use the second function
      parameter to combine any background continuation with this continuation,
      to create a new (combined) continuation.*)
  | Cont of expression * bind_kind
  | Bind of eval_monad * bind_kind

let rec bk_to_string : bind_kind -> string = function
  | Fun _ -> "Fun _"
  | Id -> "Id"
let rec evalm_to_string : eval_monad -> string = function
  | Value e -> "Value " ^ expression_to_string min_indentation e
  | Cont (e, bk) -> "Cont (" ^ expression_to_string min_indentation e ^ ", " ^
     bk_to_string bk ^ ")"
  | Bind (m, bk) -> "Bind (" ^ evalm_to_string m ^ ", " ^
     bk_to_string bk ^ ")"

(*FIXME bad style*)
let expect_value m =
  match m with
  | Value e -> e

let return_eval (e : expression) : eval_monad = Value e
let return (e : expression) : eval_continuation = fun _ ctxt -> Value e, ctxt
let continuate e f = Cont (e, Fun f)
let retry e = Cont (e, Id)

(*This serves both as the "bind" operator, and also to evaluate values inside
  the monad, to bring forward the computation.*)
let rec bind_eval normalise (m : eval_monad) (f : expression -> eval_continuation) st ctxt : eval_monad * runtime_ctxt =
  match m with
  | Value e' -> f e' st ctxt
  | Bind (em, Id) -> Bind (em, Fun f), ctxt
  | Bind (em, Fun f') ->
    begin
    let m', ctxt' = bind_eval normalise em f' st ctxt in
    match m' with
    | Value e' -> f e' st ctxt'
    | _ -> Bind (m', Fun f), ctxt'
    end
  | Cont (e, bk) ->
    begin
    let em, ctxt' = normalise st ctxt e in
    match bk with
    | Id -> Cont (e, Fun f), ctxt'
    | Fun _ -> Bind (Bind (em, bk), Fun f), ctxt'
    end

(*This is a version of bind_eval that does not "bind" computations -- rather it
  only evaluates inside the monad further.*)
and evaluate_em normalise (m : eval_monad) st ctxt : eval_monad * runtime_ctxt =
  match m with
  | Value e' -> m, ctxt
  | Bind (em, Id) -> em, ctxt
  | Bind (em, Fun f') -> bind_eval normalise em f' st ctxt
  | Cont (e, bk) ->
    begin
    let em, ctxt' = normalise st ctxt e in
    match bk with
    | Id -> em, ctxt'
    | Fun _ -> Bind (em, bk), ctxt'
    end

and run normalise st ctxt (work_list : eval_monad list) : expression list * eval_monad list * runtime_ctxt =
  List.fold_right (fun m (el, wl, ctxt) ->
    if !Config.cfg.Config.debug then print_endline (evalm_to_string m);
    match m with
    | Value e ->
      (*The value won't be fed into further continuations, so it's removed from
        the work list and added to the result list*)
      (e :: el, wl, ctxt)
    | Cont (_, Id) -> (el, m :: wl, ctxt)
    | Bind (em, Id) -> (el, em :: wl, ctxt)
    | _ ->
(* NOTE use this line instead of the following one, to see an appreciable
        degradation in memory usage. A lot of memory is leaked because of
        accumuldated closures, consisting of repeated "return" closures.
      let em, ctxt' = bind_eval normalise m return st ctxt in*)
      let em, ctxt' = evaluate_em normalise m st ctxt in
      (el, em :: wl, ctxt')) work_list ([], [], ctxt)

and run_until_done normalise st ctxt work_list results =
  if work_list = [] then results, ctxt
  else
    let el, wl, ctxt' = run normalise st ctxt work_list in
    run_until_done normalise st ctxt' wl (el @ results)
