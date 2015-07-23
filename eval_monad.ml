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

(*Monadically evaluate a list of expressions, in the style of a fold.
  A (acceptable) hack is used to stay within the monad. The hack consists of
  creating a fresh reference cell that's used to accumulate the intermediate
  values. These are then passed on to the remainder of the computation.
  Alternatively, we could have generalised the evaluation monad to include
  computations of arbitrary nature (i.e., not just normalisations of expressions)
  but that sort of generalisation did not seem warranted for what was needed here.

  "l" list down which to fold.
  "z" initial accumulator value.
  "f" fold step.
  "g" function applied to the accumulator before it's passed on to "continuation".
  "continuation" function takes an arbitrary type (i.e., OCaml, not Flick, type)
    and produces an eval_continuation.
*)
let monadic_fold (l : expression list)
      (z : 'a)
      (f : expression -> 'a -> 'a)
      (g : 'a -> 'a)
      (continuation : 'a -> eval_continuation) : eval_monad =
  let store : 'a ref = ref z in
  let expr_continuation =
    continuate flick_unit_value (fun _ st ctxt -> continuation (g !store) st ctxt) in
  List.fold_right (fun e acc ->
    continuate e (fun e st ctxt ->
      store := f e !store;
      (acc, ctxt))) l expr_continuation

(*FIXME experimental:
    - proper threading of acc through the fold
    - monadic_fold without using references*)
let monadic_fold_pure (l : expression list)
      (z : expression -> eval_continuation)
      (f : expression -> expression -> expression)
      (continuation : expression -> eval_continuation) : expression -> eval_continuation =
  List.fold_right (fun e acc ->
     fun store st ctxt ->
      continuate (f e store) (fun e st ctxt ->
       (Bind (return_eval e, (Fun acc)),
        ctxt)), ctxt) (List.rev l) z

(*Monadically evaluate a list of expressions. These are evaluated one by one,
  within the monad, then we bind with the continuation of the computation
  (again, in the monad).

  "continuation" is called last -- it is given the normalised list of
     expressions, from list "l". These would have been normalised in order*)
let continuate_list (l : expression list)
      (continuation : expression list -> eval_continuation) : eval_monad =
  monadic_fold l ([] : expression list)
    (fun e stored_es -> e :: stored_es)
    List.rev
    continuation
