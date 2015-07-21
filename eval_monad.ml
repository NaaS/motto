(*
   Evaluation monad
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)


open Crisp_syntax
open State
open Runtime_data

type ('a, 'b) eval_continuation = state -> runtime_ctxt -> ('a, 'b) eval_monad * runtime_ctxt

and ('a, 'b) bind_kind =
  | Fun of ('a -> ('a, 'b) eval_continuation)
  | Id

(*FIXME add other kinds of result values -- such as Timeout, and perhaps
        one that carries exceptions*)
and ('a, 'b) eval_monad =
    (*Expression will not be normalised any further*)
  | Value of expression
    (*Expression should be normalised further.
      If it reduces to a value in one step, then continue immediately to
      the first function parameter. Otherwise, use the second function
      parameter to combine any background continuation with this continuation,
      to create a new (combined) continuation.*)
  | Cont of expression * (expression, 'b) bind_kind
  | Return of 'b
  | Bind of ('a, 'b) eval_monad * ('a, 'b) bind_kind

let rec bk_to_string : ('a, 'b) bind_kind -> string = function
  | Fun _ -> "Fun _"
  | Id -> "Id"
let rec evalm_to_string : ('a, 'b) eval_monad -> string = function
  | Value e -> "Value " ^ expression_to_string min_indentation e
  | Cont (e, bk) -> "Cont (" ^ expression_to_string min_indentation e ^ ", " ^
     bk_to_string bk ^ ")"
  | Bind (m, bk) -> "Bind (" ^ evalm_to_string m ^ ", " ^
     bk_to_string bk ^ ")"

(*FIXME bad style*)
let expect_value m =
  match m with
  | Value e -> e

let return_eval (e : expression) : (expression, 'b) eval_monad = Value e
let return (e : expression) : (expression, 'b) eval_continuation = fun _ ctxt -> Value e, ctxt
let continuate e f : (expression, 'b) eval_monad = Cont (e, Fun f)
let retry e : (expression, 'b) eval_monad = Cont (e, Id)

(*(*A monadic version of General.fold_map*)
let fold_map (z : ('a, 'b) eval_monad * runtime_ctxt)
    (f : runtime_ctxt -> 'a -> ('a, 'b) eval_monad * runtime_ctxt)
    (l : expression list) : ( ('a, 'b) eval_monad * runtime_ctxt) =
  List.fold_right (fun e (em, ctxt) ->
    continuate e em, ctxt) l z, snd z
*)
(*This serves both as the "bind" operator, and also to evaluate values inside
  the monad, to bring forward the computation.*)
let rec bind_eval normalise (m : ('a, 'b) eval_monad) (f : 'a -> ('a, 'b) eval_continuation) st ctxt : ('a, 'b) eval_monad * runtime_ctxt =
  match m with
  | Value e' -> f e' st ctxt
  | Return x -> f x st ctxt
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
and evaluate_em normalise (m : ('a, 'b) eval_monad) st ctxt : ('a, 'b) eval_monad * runtime_ctxt =
  match m with
  | Value _ -> m, ctxt
  | Return _ -> m, ctxt
  | Bind (em, Id) -> em, ctxt
  | Bind (em, Fun f') -> bind_eval normalise em f' st ctxt
  | Cont (e, bk) ->
    begin
    let em, ctxt' = normalise st ctxt e in
    match bk with
    | Id -> em, ctxt'
    | Fun _ -> Bind (em, bk), ctxt'
    end

and run normalise st ctxt (work_list : ('a, 'b) eval_monad list) : expression list * ('a, 'b) eval_monad list * runtime_ctxt =
  List.fold_right (fun m (el, wl, ctxt) ->
    if !Config.cfg.Config.debug then print_endline (evalm_to_string m);
    match m with
    | Value e ->
      (*The value won't be fed into further continuations, so it's removed from
        the work list and added to the result list*)
      (e :: el, wl, ctxt)
    | Return _ ->
      (*'Return' values should not be exposed -- they are meant for
        communicating partly-evaluated non-expression results (e.g., lists of
        expressions) in between schedules. Once the result has been fully
        evaluated, it's expected that it will be used to generate an expression,
        which will ultimately be normalised to a Value.*)
      failwith "Monad produced 'Return' value"
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
