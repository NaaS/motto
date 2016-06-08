(*
   Evaluator for Flick programs.
   Nik Sultana, Cambridge University Computer Lab, July 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open General
open Debug
open Crisp_syntax
open State
open Runtime_data
open Eval_monad

(*FIXME include runtime_ctxt in state?*)
exception Eval_Exc of string * expression option * typed_value option (** state -- FIXME include runtime_ctxt*)

exception Empty_Channel of runtime_ctxt

(*Translate a normal expression into a value*)
let rec evaluate_value (ctxt : runtime_ctxt) (e : expression) : typed_value =
  match e with
  | Crisp_syntax.Str s -> String s
  | Crisp_syntax.Int i -> Integer i
  | Crisp_syntax.True -> Boolean true
  | Crisp_syntax.False -> Boolean false
  | Crisp_syntax.IPv4_address (i1, i2, i3, i4) -> IPv4Address (i1, i2, i3, i4)
  | EmptyList -> List []
  | ConsList (h, t) ->
    let h' = evaluate_value ctxt h in
    let t' =
      match evaluate_value ctxt t with
      | List xs -> xs
      | _ ->
        let t_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation t in
        raise (Eval_Exc ("Tail expression " ^ t_s ^ " did not evaluate to a list", Some e, None)) in
    List (h' :: t')
  | Crisp_syntax.TupleValue es ->
    Tuple (List.map (evaluate_value ctxt) es)
  | Crisp_syntax.Record vs ->
    let vs' = List.map (fun (l, v) -> (l, evaluate_value ctxt v)) vs in
    RecordType vs'
  | Crisp_syntax.Functor_App (l, [Crisp_syntax.Exp e]) ->
    (*FIXME would be used to check that functor "l" is indeed a disjunct.
            if state is passed to this function then could quickly check that l's
            identifier_kind is Disjunct*)
    Disjoint_Union (l, evaluate_value ctxt e)
  | TypeAnnotation (e', _) -> evaluate_value ctxt e'
  | _ ->
    raise (Eval_Exc ("Cannot represent as Flick expression. Perhaps it's not in normal form?", Some e, None))

exception Unavailue (*Data we seek is unavailable. We'll try again later.*)

(*Translate a value into an expression*)
let rec devaluate (v : typed_value) : expression =
  match v with
  | UserDefinedType (_, v) -> devaluate v
  | String s -> Crisp_syntax.Str s
  | Integer i -> Crisp_syntax.Int i
  | Boolean b -> if b then Crisp_syntax.True else Crisp_syntax.False
  | IPv4Address (i1, i2, i3, i4) ->
    Crisp_syntax.IPv4_address (i1, i2, i3, i4)
  | List vs ->
    List.map devaluate vs
    |> List.rev
    |> (fun vs' -> List.fold_right (fun e l ->
      ConsList (e, l)) vs' EmptyList)
  | Tuple vs ->
    Crisp_syntax.TupleValue (List.map devaluate vs)
  | RecordType vs ->
    let vs' = List.map (fun (l, v) -> (l, devaluate v)) vs in
    Crisp_syntax.Record vs'
  | Disjoint_Union (l, v) -> Crisp_syntax.Functor_App (l, [Crisp_syntax.Exp (devaluate v)])
  | Dictionary _ ->
    (*FIXME could serialise as an association list?*)
    raise (Eval_Exc ("Cannot represent as Flick expression", None, Some v))
  | ChanType (cn, _) -> Variable cn
  | Resource (Reference_resource r) ->
    match r.retrieve () with
    | Expression e -> e
    | Unavailable ->
      (*FIXME this is not caught properly at the moment*)
      raise Unavailue
    | Error s ->
      raise (Eval_Exc ("(Resource error) " ^ s, None, Some v))

(*NOTE b and l should be in normal form*)
let rec fold_list ?acc:(acc : expression option = None)
  (f : expression -> expression -> expression) (b : expression) (l : expression) : expression =
  let acc =
    match acc with
    | None -> b
    | Some acc -> acc in
  match l with
  | EmptyList -> acc
  | ConsList (h, t) -> fold_list ~acc:(Some (f h acc)) f b t
  | _ ->
    raise (Eval_Exc ("fold_list : not given a list", Some l, None))

let iterated_cons (es : expression list) (l : expression) : expression =
  begin
    (*NOTE this is only a shallow check -- we don't check the rest of the list
           to ensure that it's well-formed (i.e., consists only of ConList
           applications finishing with EmptyList)*)
    match l with
    | EmptyList -> ()
    | ConsList (_, _) -> ()
    | _ ->
      raise (Eval_Exc ("iterated_cons : not given a list", Some l, None))
  end;
  List.fold_right (fun x l -> ConsList (x, l)) (List.rev es) l

(*NOTE l1 should be in normal form*)
let rec append_list (acc : expression list)
   (l1 : expression) (l2 : expression) : expression =
  match l1 with
  | EmptyList -> iterated_cons acc l2
  | ConsList (h, EmptyList) -> iterated_cons acc (ConsList (h, l2))
  | ConsList (h, t) -> append_list (h :: acc) t l2
  | _ ->
    raise (Eval_Exc ("append_list : not given a list", Some l1, None))

(*Convert Flick list into an OCaml list of expressions.
  NOTE it's assumed that the Flick list is normal (at least up to the surface
       list level).*)
let interpret_flick_list (e : expression) : expression list =
  let rec interpret_flick_list' (acc : expression list) (e : expression) : expression list =
  match e with
  | EmptyList -> List.rev acc
  | ConsList (x, l) -> interpret_flick_list' (x :: acc) l
  | _ ->
    raise (Eval_Exc ("interpret_flick_list : not given a list", Some e, None)) in
  interpret_flick_list' [] e

let get_dictionary caller_form e v ctxt =
  if not (List.mem_assoc v ctxt.Runtime_data.value_table) then
    raise (Eval_Exc ("Cannot " ^ caller_form ^ ": Symbol " ^ v ^ " not in runtime context", Some e, None));
  match List.assoc v ctxt.Runtime_data.value_table with
  | Dictionary d -> d
  | _ ->
   raise (Eval_Exc ("Cannot " ^ caller_form ^ ": Symbol " ^ v ^ " not a dictionary ", Some e, None))

(*Get some value from the runtime context*)
let resolve ctxt l =
  match List.filter (fun (name, _) -> name = l) ctxt.value_table with
  | [] ->
    raise (Eval_Exc ("Cannot resolve variable '" ^ l ^
                     "' in the value table", None, None))
  | [(_, v)] -> v
  | results ->
    let results_s =
      strlist_of_value_table results
      |> print_list runtime_ctxt_print_indentation in
    raise (Eval_Exc ("Multiple resolutions for variable's value: " ^
                     results_s, None, None))

(*Reduce an expression into a value expression*)
let rec normalise (st : state) (ctxt : runtime_ctxt) (e : expression) : eval_monad * runtime_ctxt =
  match e with
  (*These expressions are already normal*)
  | True
  | False
  | Int _
  | IPv4_address _
  | Str _
  | EmptyList -> return_eval e, ctxt

  | Variable l ->
    let e' =
      try resolve ctxt l with
      | Eval_Exc (s, None, None) ->
        raise (Eval_Exc (s, Some e, None)) in
    (*FIXME do we need to check if l is a reference? might need to do this to
            treat it differently depending on whether we want the reference or
            its dereference. (the latter corresponds to automatically inserting
            coercions from references to values.)
            Perhaps the 'normalise' function can be given an optional parameter
            specifying whether a value or a reference is expected?
            To answer this i need to look at how the lvalue is handled in
            assignment expressions.*)
    return_eval (devaluate e'), ctxt
  | InvertedVariable _ -> return_eval e, ctxt

  | TypeAnnotation (e', _) ->
    (*NOTE there's no runtime type-checking -- we ignore the type annotation.
           that should have been checked at an earlier pass.*)
    normalise st ctxt e'

  | Not e' ->
    continuate e' (fun e' st ctxt ->
      match e' with
      | True -> return_eval False, ctxt
      | False -> return_eval True, ctxt
      | _ ->
        raise (Eval_Exc ("Cannot normalise to Boolean value", Some e', None))), ctxt

  | And (e1, e2)
  | Or (e1, e2) ->
    begin
    let f =
      match e with
      | And (_, _) -> (fun b1 b2 -> if b1 = True && b2 = True then True else False)
      | Or (_, _) -> (fun b1 b2 -> if b1 = True || b2 = True then True else False)
      | _ -> failwith "Impossible" in

    continuate e1 (fun b1 st ctxt ->
      continuate e2 (fun b2 st ctxt' ->
        match b1, b2 with
        | True, True
        | False, True
        | True, False
        | False, False -> return_eval (f b1 b2), ctxt'
        | anomalous, True
        | anomalous, False ->
          let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
          raise (Eval_Exc ("Cannot normalise to Boolean value. Got " ^ anomalous_s, Some e1, None))
        | True, anomalous
        | False, anomalous ->
          let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
          raise (Eval_Exc ("Cannot normalise to Boolean value. Got " ^ anomalous_s, Some e2, None))
        | _, _ ->
          raise (Eval_Exc ("Cannot normalise to Boolean value", Some e, None))), ctxt), ctxt
    end

  | Equals (e1, e2) ->
    continuate e1 (fun e1' st ctxt ->
      continuate e2 (fun e2' st ctxt' ->
        let result = if e1' = e2' then True else False in
        (return_eval result, ctxt')), ctxt), ctxt

  | GreaterThan (e1, e2)
  | LessThan (e1, e2) ->
    begin
    let f =
      match e with
      | GreaterThan _ -> (fun i1 i2 -> if i1 > i2 then True else False)
      | LessThan _ -> (fun i1 i2 -> if i1 < i2 then True else False)
      | _ -> failwith "Impossible" in
    continuate e1 (fun e1' st ctxt ->
      continuate e2 (fun e2' st ctxt' ->
        match e1', e2' with
        | Int i1, Int i2 -> return_eval (f i1 i2), ctxt'
        | anomalous, Int _ ->
          let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
          raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e1, None))
        | Int _, anomalous ->
          let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
          raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e2, None))
        | _, _->
          raise (Eval_Exc ("Cannot normalise to integer value", Some e, None))), ctxt), ctxt
    end

  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | IntegerRange (e1, e2) ->
    begin
    let f =
      match e with
      | Plus _ -> (fun i1 i2 -> Int (i1 + i2))
      | Minus _ -> (fun i1 i2 -> Int (i1 - i2))
      | Times _ -> (fun i1 i2 -> Int (i1 * i2))
      | Mod _ -> failwith "TODO"
                   (*FIXME support more from http://caml.inria.fr/pub/docs/manual-ocaml/libref/Int32.html*)
      | Quotient _ ->
        (fun i1 i2 ->
           (*FIXME need better handling for 'division by zero'*)
           if i2 = 0 then failwith "DbZ";
           Int (i1 / i2))
      | IntegerRange _ -> (fun i1 i2 ->
          enlist i1 (i2 + 1)
          |> Crisp_syntax_aux.flick_integer_list)
      | _ -> failwith "Impossible" in

    continuate e1 (fun e1' st ctxt ->
      continuate e2 (fun e2' st ctxt' ->
        match e1', e2' with
        | Int i1, Int i2 -> return_eval (f i1 i2), ctxt'
        | anomalous, Int _ ->
          let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
          raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e1, None))
        | Int _, anomalous ->
          let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
          raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e2, None))
        | _, _->
          raise (Eval_Exc ("Cannot normalise to integer value", Some e, None))), ctxt), ctxt
    end

  | Abs e' ->
    continuate e' (fun e' st ctxt' ->
      match e' with
      | Int i -> return_eval (Int (abs i)), ctxt'
      | anomalous ->
        let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
        raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e', None))), ctxt

  | Int_to_address e' ->
    continuate e' (fun e' st ctxt' ->
      match e' with
      | Int i -> failwith "TODO"
      | anomalous ->
        let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
        raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e', None))), ctxt
  | Address_to_int e' ->
    continuate e' (fun e' st ctxt' ->
      match e' with
      | IPv4_address (i1, i2, i3, i4) -> failwith "TODO"
      | anomalous ->
        let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
        raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e', None))), ctxt

  | ConsList (h, t) ->
    continuate h (fun h' st ctxt ->
      continuate t (fun t' st ctxt' ->
        return_eval (ConsList (h', t')), ctxt'), ctxt), ctxt
  | AppendList (l1, l2) ->
    continuate l1 (fun l1' st ctxt ->
      continuate l2 (fun l2' st ctxt' ->
        return_eval (append_list [] l1' l2'), ctxt'), ctxt), ctxt

  | ITE (b, e1, e2_opt) ->
    continuate b (fun b st ctxt' ->
      match b with
      | True -> normalise st ctxt' e1
      | False ->
        begin
        match e2_opt with
        | None -> return_eval Crisp_syntax.flick_unit_value, ctxt'
        | Some e2 -> normalise st ctxt' e2
        end
      | anomalous ->
        let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
        raise (Eval_Exc ("Cannot normalise to Boolean value. Got " ^ anomalous_s, Some b, None))), ctxt

  | TupleValue es ->
    if es = [] then
      (*unit value does not need further normalisation*)
      return_eval (TupleValue es), ctxt
    else
      continuate_list es (fun es' _ ctxt' ->
        return_eval (TupleValue es'), ctxt'), ctxt

  | Record fields ->
    let labels, es = List.split fields in
    continuate_list es (fun es' _ ctxt' ->
      let fields' = List.combine labels es' in
      return_eval (Record fields'), ctxt'), ctxt

  | CaseOf (e', cases) ->
    continuate e' (fun e'norm st ctxt' ->
      let disj, body =
        match e'norm with
        | Functor_App (f_name, [Exp e]) ->
          (*NOTE would be prudent to check if the functor is indeed a disjunct*)
          (f_name, e)
        | anomalous ->
          let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
          raise (Eval_Exc ("Cannot normalise to disjunct. Got " ^ anomalous_s, Some e', None)) in

      continuate body (fun arg st ctxt'' ->
        match List.filter (fun (h, _) ->
          (*NOTE should not have to normalise h -- it's used as a pattern*)
          match h with
          | Functor_App (f_name, [Exp _]) -> f_name = disj
          | anomalous ->
            let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
            raise (Eval_Exc ("This should be a disjunct. Got " ^ anomalous_s, Some h, None))) cases with
        | [] ->
          let e'_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation e' in
          raise (Eval_Exc ("Could not find case for this normal disjunct: " ^ e'_s, Some e'norm, None))
        | [(Functor_App (_, [Exp (Variable v)]), body_e)] ->
          (*Replace "Variable v" with "arg" in body_e*)
          Crisp_syntax_aux.subst_var v arg body_e
          |> normalise st ctxt''
        | _ ->
          let e'_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation e' in
          raise (Eval_Exc ("Found multiple cases for this normal disjunct: " ^ e'_s, Some e'norm, None))), ctxt'), ctxt

  (*This work for both tuples and records.*)
  | RecordProjection (e', l) ->
    continuate e' (fun e'norm st ctxt' ->
      let e'norm_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation e'norm in
      let project_from fields =
        match List.filter (fun (field_name, _) -> field_name = l) fields with
        | [] ->
          raise (Eval_Exc ("Cannot find label called " ^ l ^ " in expression: " ^ e'norm_s, Some e, None))
        | [(_, field_e)] -> normalise st ctxt' field_e
        | _ ->
          raise (Eval_Exc ("Found multiple labels called " ^ l ^ " in expression: " ^ e'norm_s, Some e, None)) in
      match e'norm with
      | Record fields -> project_from fields
      | TupleValue es ->
        let labels =
          enlist 1 (List.length es + 1)
          |> List.map string_of_int in
        List.combine labels es
        |> project_from
      | _ ->
        raise (Eval_Exc ("Cannot project from this normal expression: " ^ e'norm_s, Some e, None))), ctxt

  | RecordUpdate (record_e, (field_name, field_body_e)) ->
    continuate record_e (fun e'norm st ctxt' ->
      continuate field_body_e (fun field_body_e' st ctxt'' ->
        let e'norm_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation e'norm in
        match e'norm with
        | Record fields ->
          (*fields have already been normalised, since they're contained in record_e*)
          let updated, fields' =
            List.fold_right (fun ((field_l, field_e) as field) (updated, field_acc) ->
              if field_l = field_name then
                if updated then
                  raise (Eval_Exc ("Cannot record-update this normal expression: " ^ e'norm_s, Some e, None))
                else
                  (true, (field_name, field_body_e') :: field_acc)
              else (updated, field :: field_acc)) (List.rev fields) (false, []) in
          if updated then
            return_eval (Record fields'), ctxt''
          else
            raise (Eval_Exc ("Could not find field name " ^ field_name ^ " to update in this normal expression: " ^ e'norm_s, Some e, None))
        | _ ->
          raise (Eval_Exc ("Cannot record-update this normal expression: " ^ e'norm_s, Some e, None))), ctxt'), ctxt

  | Map (v, l, body, unordered) ->
    (*FIXME "unordered" not taken into account. its value will affect the
            monadic evaluator.*)
    continuate l (fun l' st ctxt' ->
      let l' = interpret_flick_list l' in
      continuate_list (List.map (fun e -> Crisp_syntax_aux.subst_var v e body) l')
        (fun l'' st ctxt'' ->
          return_eval (Crisp_syntax_aux.flick_list l''), ctxt''), ctxt'), ctxt

  | Iterate (v, l, acc_opt, body, unordered) ->
    (*FIXME "unordered" not taken into account. its value will affect the
            monadic evaluator.*)
    continuate l (fun l' st ctxt' ->
      let l' = interpret_flick_list l' in
      let bodies = List.map (fun e -> Crisp_syntax_aux.subst_var v e body) l' in
      match acc_opt with
      | None ->
        monadic_map bodies
          flick_unit_value
          (fun _ unity -> unity)
          (fun (e : expression) -> e)
          (fun (e : expression) st ctxt ->
             Cont (e, Id)(*FIXME use "retry" here?
                                 "retry" doesn't feel like the right name for
                                   the idiom*), ctxt), ctxt'
      | Some (acc_l, acc_e) ->
        continuate acc_e
          (monadic_fold bodies
            return
            (fun body acc_e' -> Crisp_syntax_aux.subst_var acc_l acc_e' body)
            return), ctxt'), ctxt

  | Functor_App (function_name, fun_args) ->
    (*NOTE local and global state are handled very naively: we just assume that
           the type-checker has ensured that the function/process isn't accessing
           any state that it shouldn't; here we just allow the function/process
           to interact directly with the runtime context, unchecked.
           Thus we don't do anything to "connect" or "disconnect" state.*)
    (*This is used both to call functions and processes. (In the case of a process,
      its body goes through a single iteration; upon reaching the end of its evaluation,
      control is returned to the caller. Usually i'd expect that caller to be
      the scheduler. A scheduler repeatedly calls processes according to some
      schedule.)
      First we extract the function/process info from the relevant runtime context.
      Then:
        - we don't need to create fresh scope for values that aren't passed by
          reference (to avoid having their updates leaking to containing scopes).
          instead, we "undefine" those variables when the function evaluation ends.
          (only by-reference values should have their updates leaked, so we don't
           undefine such variables.)
        - set up handlers for exceptions, and "connect" function's state.
          there are two kinds of state: local and global.
          Global state consists of reference cells that other functions/processes
          may read and change. Local state consists of static variables:
          reference cells that only invocations of this function/process may
          read and change.
        - after the function body has been normalised, we undefine its local variables,
          unwind exception handlers, and "disconnect" the function's state; its
          state should survive until the next invocation.*)
    begin
    (*Determine whether this is a function call, or disjunct.
      If the latter, then it's already a value -- cannot be reduced further.*)
    match lookup_term_data (Term Undetermined) st.term_symbols function_name with
    | None ->
      raise (Eval_Exc ("Could not retrieve metadata from symbol table, for functor: " ^ function_name, Some e, None))
    | Some (_, {identifier_kind; _}) ->
      begin
      match identifier_kind with
      | Disjunct _ -> return_eval e, ctxt
      | Defined_Function_Name ->
        let arg_es = List.map (function
            | Exp e -> e
            | Named _ -> failwith "TODO") fun_args in
        continuate_list arg_es (fun normal_arg_es _ ctxt' ->
          return_eval (Functions.apply_function function_name normal_arg_es), ctxt'
        ), ctxt
      | Function_Name ->
        let arg_es = List.map (function
            | Exp e -> e
            | Named _ -> failwith "TODO") fun_args in
        continuate_list arg_es (fun normal_arg_es st ctxt' ->
          let (stata, body, excs) =
            (*Get function implementation*)
            match List.filter (fun (name, _) -> name = function_name) ctxt'.exec_table with
            | [] ->
              raise (Eval_Exc ("Could not retrieve implementation from runtime context, for functor: " ^ function_name, Some e, None))
            | [(_, Function {fn_body; _})] ->
              begin
              match fn_body with
              | ProcessBody (stata, body, excs) -> (stata, body, excs)
              end
            | [(_, Process {process_body; _})] ->
              raise (Eval_Exc ("Calling processes not supported, for functor: " ^ function_name, Some e, None))
            | _ ->
              raise (Eval_Exc ("Invalid declaration found when calling functor:" ^ function_name, Some e, None)) in
            let (is_fun(*FIXME this info is not yet used*),
                 (dis(*FIXME this info is not yet used*), (chans, arg_tys), ret_tys)) =
            match lookup_function_type st function_name with
            | None ->
              raise (Eval_Exc ("Could not retrieve type from symbol table, for functor: " ^ function_name, Some e, None))
            | Some (is_fun, ft) ->
              (is_fun, Crisp_syntax_aux.extract_function_types ft) in
          assert is_fun; (*FIXME currently this only works for functions*)
          let arg_tys =
            (*Regard channels as simply being parameters*)
            List.map Crisp_syntax_aux.chan_to_ty chans @ arg_tys in
          let formal_arg_names =
            List.map (fun ty ->
              match Crisp_syntax_aux.label_of_type ty with
              | None ->
                let ty_s = type_value_to_string true false min_indentation ty in
                raise (Eval_Exc ("Missing label for parameter typed " ^ ty_s ^ " for functor " ^ function_name, Some e, None))
              | Some label -> label) arg_tys in

          let ctxt'' =
            { ctxt' with except_table = excs :: ctxt'.except_table } in

          (*Substitute formal for actual parameters in the function body*)
          let body' =
            List.fold_right (fun (v, arg) body ->
              Crisp_syntax_aux.subst_var v arg body)
              (List.combine formal_arg_names normal_arg_es) body in

          if !Config.cfg.Config.verbosity > 0 then
            begin
              print_endline "Function application. Arguments:";
              List.combine formal_arg_names normal_arg_es
              |> List.iter (fun (v, e) ->
                print_endline ("  " ^ v ^ " |-> " ^ expression_to_string 0 e));
              print_endline "Function application. Body:";
              print_endline (expression_to_string 2 body')
            end;

          continuate body' (fun result st ctxt'' ->
            (*Unwind exception handlers*)
            let ctxt''' =
              { ctxt'' with except_table =
                  match ctxt''.except_table with
                  | [] -> failwith "Impossible" (*because we just extended the exception stack*)
                  | _ :: rest -> rest } in

            (*Undefine local variables*)
            (*NOTE i originally thought that the variables to reset are those in
                   the set "bound_vars \ state vars", but actually we just need to
                   look among the bound variables*)
            let ctxt4 =
              Crisp_syntax_aux.bound_vars e []
              |> (fun l -> List.fold_right Runtime_data.undefine_value l ctxt''') in

            return_eval result, ctxt4), ctxt''), ctxt
    | _ ->
      raise (Eval_Exc ("Functor " ^ function_name ^ " had inconsistent identifier kind " ^
             string_of_identifier_kind identifier_kind, Some e, None))
      end
    end

  | LocalDef ((v, _), e) ->
    continuate e (fun e' st ctxt' ->
      let ctxt'' = { ctxt' with
        value_table =
          let pair = (v, evaluate_value ctxt' e') in
          add_unique_assoc pair ctxt'.value_table } in
      return_eval e', ctxt''), ctxt

  | Update (v, e) ->
    (*NOTE this handler's implementation is very similar to that for Set in
           Runtime_inspect.eval, except that here we don't do any type-checking:
           that would have been done in earlier parts of the compiler pipeline
           if we're compiling, or by the interpreter if we're interpreting.*)
    continuate e (fun e' st ctxt' ->
      let updated_value = evaluate_value ctxt' e' in
      let next =
        (*FIXME horrible code style*)
        try
          (*Update runtime context*)
          let ctxt'' =
            if not (List.mem_assoc v ctxt'.Runtime_data.value_table) then
              raise (Eval_Exc ("Cannot Update: Symbol " ^ v ^ " not in runtime context", Some e, None));
            let pair, e' =
              let current_value = List.assoc v ctxt'.Runtime_data.value_table in
              match current_value with
              | Resource (Reference_resource r) ->
                begin
                match r.update e' with
                | Expression e' ->
                  (*Extract the value from the reference, but leave the reference
                    as it is -- i.e., referring to an external "back-patched"
                    resource*)
                  (v, current_value), e'
                | Unavailable ->
                  (*FIXME retry until timeout of some sort*)
                  raise Unavailue
                | Error s ->
                  raise (Eval_Exc ("Cannot Update (resource error): " ^ s, Some e, None));
                end
              | Resource r ->
                raise (Eval_Exc ("Cannot Update: Symbol " ^ v ^
                                 " is wrong type of resource: " ^
                                 Resources.string_of_resource r, Some e, None));
              | _ -> (v, updated_value), e' in
            { ctxt' with Runtime_data.value_table =
                General.add_unique_assoc pair ctxt'.Runtime_data.value_table } in
          return_eval e', ctxt''
        with Unavailue ->
          (*We cannot progress in the computation at this time , so try again later.*)
          (*FIXME timeout?*)
          (retry e, ctxt) in
    next), ctxt

  | UpdateIndexable (v, idx, e) ->
    let dict = get_dictionary "UpdateIndexable" e v ctxt in

    let idx_v, ctxt' = evaluate st ctxt idx in

    continuate e (fun e' st ctxt'' ->
      let e_v = evaluate_value ctxt'' e' in
      let dict' = General.add_unique_assoc (idx_v, e_v) dict in

      if not (List.mem_assoc v ctxt''.Runtime_data.value_table) then
        raise (Eval_Exc ("Cannot UpdateIndexable: Symbol " ^ v ^ " not in runtime context", Some e, None));

      let ctxt''' =
        { ctxt'' with Runtime_data.value_table =
            let pair = (v, Runtime_data.Dictionary dict') in
            General.add_unique_assoc pair ctxt''.Runtime_data.value_table } in
      return_eval e', ctxt'''), ctxt'

  | IndexableProjection (v, idx) ->
    (*FIXME how to initialise dictionaries etc in the simulator?*)
    let dict = get_dictionary "IndexableProjection" e v ctxt in

    let idx_v, ctxt' = evaluate st ctxt idx in

    if not (List.mem_assoc idx_v dict) then
      raise (Eval_Exc ("Cannot IndexableProjection: Key " ^ string_of_typed_value idx_v ^ " not found in dictionary " ^ v, Some e, None));

    let e =
      List.assoc idx_v dict
      |> devaluate in
    return_eval e, ctxt'

  | Send (inv, chan_ident, e') ->
    continuate e' (fun e' st ctxt ->
    (*FIXME currently we assume that channels have infinite capacity, but this
            assumption should be revised. When channels are annotated with some
            finite capacity, then Send will block when that channel's capacity
            has been reached.*)
    let e_value = evaluate_value ctxt e' in

    let f dir (ctxt : runtime_ctxt) (incoming, outgoing) =
      match dir with
      | Incoming ->
        if not inv then
          raise (Eval_Exc ("Unexpected direction: Send only works in the outgoing direction", Some e, None))
        else (List.rev (e_value :: List.rev incoming), outgoing, e_value, ctxt)
      | Outgoing ->
        if not inv then
          (incoming, List.rev (e_value :: List.rev outgoing), e_value, ctxt)
        else
          raise (Eval_Exc ("Unexpected direction: inverted Send only works in the incoming direction", Some e, None)) in
    channel_fun_ident chan_ident
      (if not inv then "send" else "inverted send")
      (if not inv then Outgoing else Incoming)
      (fun s -> Eval_Exc (s, Some e, None)) f st ctxt
    |> apfst return_eval), ctxt

  | Receive (inv, chan_ident) ->
    begin
    let f dir (ctxt : runtime_ctxt) (incoming, outgoing) =
      match dir with
      | Incoming ->
        if not inv then
          match incoming with
          | v :: xs -> xs, outgoing, v, ctxt
          | [] -> raise (Empty_Channel ctxt)
        else
          raise (Eval_Exc ("Unexpected direction: inverted Receive only works in the outgoing direction", Some e, None))
      | Outgoing ->
        if not inv then
          raise (Eval_Exc ("Unexpected direction: Receive only works in the incoming direction", Some e, None))
        else
          match outgoing with
          | v :: xs -> incoming, xs, v, ctxt
          | [] -> raise (Empty_Channel ctxt) in
    try
      let v, ctxt' =
        channel_fun_ident chan_ident
          (if not inv then "receive" else "inverted receive")
          (if not inv then Incoming else Outgoing)
          (fun s -> Eval_Exc (s, Some e, None)) f st ctxt in
      (return_eval v, ctxt')
    with
    | Empty_Channel _(*FIXME was "ctxt", but should ignore any changes done to the context*) ->
      (retry e, ctxt)
    end
  | Peek (inv, chan_ident) ->
    begin
    let f dir (ctxt : runtime_ctxt) (incoming, outgoing) =
      match dir with
      | Incoming ->
        if not inv then
          match incoming with
          | v :: _ -> incoming, outgoing, v, ctxt
          | [] -> raise (Empty_Channel ctxt)
        else
          raise (Eval_Exc ("Unexpected direction: inverted Peek only works in the outgoing direction", Some e, None))
      | Outgoing ->
        if not inv then
          raise (Eval_Exc ("Unexpected direction: Peek only works in the incoming direction", Some e, None))
        else
          match outgoing with
          | v :: _ -> incoming, outgoing, v, ctxt
          | [] -> raise (Empty_Channel ctxt) in
    try
      let v, ctxt' =
        channel_fun_ident chan_ident
          (if not inv then "peek" else "inverted peek")
          (if not inv then Incoming else Outgoing)
          (fun s -> Eval_Exc (s, Some e, None)) f st ctxt in
      (return_eval v, ctxt')
    with
    | Empty_Channel _(*FIXME was "ctxt", but should ignore any changes done to the context*) ->
      (retry e, ctxt)
    end

  | Seq (TypeAnnotation (Meta_quoted mis, _), e') ->
    List.iter (fun mi ->
      match mi with
      | Show_symbol_table None
      | Show_symbol_table (Some Runtime_phase) ->
        print_endline (State_aux.state_to_str false st)
      | PrintStr (None, s)
      | PrintStr (Some Runtime_phase, s) -> print_endline s
      | Show_runtime_ctxt None
      | Show_runtime_ctxt (Some Runtime_phase) ->
        print_endline (Runtime_data.string_of_runtime_ctxt ctxt)
      | _ -> () (*ignore other MIs, since they're not relevant to this part of the compiler*)) mis;
    normalise st ctxt e'

  | Meta_quoted _ ->
    raise (Eval_Exc ("Cannot normalise meta_quoted expressions alone -- add some other expression after them, and normalisation should succeed.", Some e, None))

  | Seq (e1, e2) ->
    continuate e1 (fun _ _ ctxt -> normalise st ctxt e2), ctxt

  | Hole -> raise (Eval_Exc ("Cannot normalise", Some e, None))

  | Can e ->
    (*FIXME this could be improved by extracing the list of things that might go
            wrong from an arbitrary expression "e", and check them one by one.
            Currently we require "Can" to be affixed immediately to specific
            expressions.*)
    begin
      match e with
      | IndexableProjection (v, idx) ->
        let dict = get_dictionary "IndexableProjection" e v ctxt in
        let idx_v, ctxt' = evaluate st ctxt idx in
        let e =
          if List.mem_assoc idx_v dict then Crisp_syntax.True
          else Crisp_syntax.False in
        return_eval e, ctxt
      | Receive (inv, chan_ident)
      | Peek (inv, chan_ident) ->
        begin
        let f dir (ctxt : runtime_ctxt) (incoming, outgoing) =
          begin
          match dir with
          | Incoming ->
            if not inv then
              match incoming with
              | _ :: _ -> incoming, outgoing, Boolean true, ctxt
              | [] -> incoming, outgoing, Boolean false, ctxt
            else
              raise (Eval_Exc ("Unexpected direction: inverted Receive only works in the outgoing direction", Some e, None))
          | Outgoing ->
            if not inv then
              raise (Eval_Exc ("Unexpected direction: Receive only works in the incoming direction", Some e, None))
            else
              begin
              match outgoing with
              | _ :: _ -> incoming, outgoing, Boolean true, ctxt
              | [] -> incoming, outgoing, Boolean false, ctxt
              end
          end in
        let v, ctxt' =
          channel_fun_ident chan_ident
            (if not inv then "receive" else "inverted receive")
            (if not inv then Incoming else Outgoing)
            (fun s -> Eval_Exc (s, Some e, None)) f st ctxt in
        (return_eval v, ctxt')
        end
      | Send _ ->
        (*FIXME crude -- in this runtime we can always send on a channel since
                we assume an infinitely-sized buffer*)
        (return_eval Crisp_syntax.True, ctxt)


      | Variable l ->
        begin
        match resolve ctxt l with
        | Resource r ->
          let result =
            if Resources.resource_is_available r then Crisp_syntax.True
            else Crisp_syntax.False in
          (return_eval result, ctxt)
        | _ -> failwith "'can' cannot be applied to this kind of identifier" (*FIXME give more info*)
        end

      | _ -> failwith "TODO"
    end

  | Size e ->
    (*FIXME this could be improved by extracing the list of things that might go
            wrong from an arbitrary expression "e", and check them one by one.
            Currently we require "Can" to be affixed immediately to specific
            expressions.*)
    begin
      match e with
      | Variable l ->
        let len =
          match resolve ctxt l with
          | String s -> String.length s
          | List vs -> List.length vs
          | Dictionary dict -> List.length dict
          | ChanType (_, cty) ->
            begin
            match cty with
            | ChannelSingle _ -> 1
            | ChannelArray chans ->
              (*NOTE we return the current number of channels. this should be
                     the same number that the channel type was declared to have*)
              List.length chans
(* If we wanted to work off the type's declaration instead, we would have done
   something like this:
            | Crisp_syntax.ChannelArray (_, _, di_opt) ->
              match di_opt with
              | None -> 0 (*FIXME do we need to dimension such arrays dynamically?*)
              | Some di -> resolve_di di
*)
            end in
        let e = Crisp_syntax.Int len in
        return_eval e, ctxt
      | Receive (inv, chan_ident)
      | Peek (inv, chan_ident)
      | Send (inv, chan_ident, _) ->
        begin
        let direction =
          match e with
          | Receive _
          | Peek _ -> if not inv then Incoming else Outgoing
          | Send _ -> if not inv then Outgoing else Incoming in
        let op_name =
          match e with
          | Receive _ -> if not inv then "receive" else "inverted receive"
          | Peek _ -> if not inv then "peek" else "inverted peek"
          | Send _ -> if not inv then "send" else "inverted send" in
        let f dir (ctxt : runtime_ctxt) (incoming, outgoing) =
          match dir with
          | Incoming ->
            incoming, outgoing, Integer (List.length incoming), ctxt
          | Outgoing ->
            incoming, outgoing, Integer (List.length outgoing), ctxt in
        let v, ctxt' =
          channel_fun_ident chan_ident
            op_name
            direction
            (fun s -> Eval_Exc (s, Some e, None)) f st ctxt in
        (return_eval v, ctxt')
        end

      | _ -> failwith "TODO"
    end

(*Lift a function over channels to operate on a channel_identifier.
  It factors out the lookup-related boilerplate for channels.*)
and channel_fun_ident ((c_name, idx_opt) : channel_identifier) (operation_verb : string)
      (dir : channel_direction)
      (operation_exn : string -> exn)
      (f : channel_direction -> runtime_ctxt -> (typed_value list * typed_value list) ->
       (typed_value list * typed_value list * typed_value * runtime_ctxt)) st ctxt =
    (*chan_e can either be IndexableProjection or Variable,
      depending on whether c_name refers to a ChannelArray or ChannelSingle *)
    let chan_e =
      match idx_opt with
      | None -> Variable c_name
      | Some idx -> IndexableProjection (c_name, idx) in
    (*NOTE don't call normalise on the resulting expression, since we won't
           be able to devaluate the Variable value (since we cannot represent
           channels as Flick values*)

    let v, idx_opt, ctxt' =
      match chan_e with
      | Variable v -> v, None, ctxt
      | IndexableProjection (v, idx_e) ->
        begin
        let idx, ctxt' =
          evaluate st ctxt idx_e in
        let idx =
          match idx with
          (*FIXME here assuming that channel arrays are indexable only by integers*)
          | Runtime_data.Integer i -> i
          | _ ->
            raise (Eval_Exc ("Invalid channel index", Some idx_e, None)) in
        v, Some idx, ctxt'
        end
      | _ ->
        raise (operation_exn "Cannot Send: cannot extract channel reference") in

    let ctxt'', e_value' =
      let ctxt, pre_e_value' =
        channel_fun v dir idx_opt operation_verb operation_exn f st ctxt' in

      let e_value' =
        match pre_e_value' with
        | Some e -> e
        | None -> failwith "Impossible" in
      (ctxt, e_value') in

    let e''' = devaluate e_value' in
    e''', ctxt''

(*Translate an arbitrary expression into a value*)
and evaluate ?work_item_name:(work_item_name : work_item_name = "<unnamed>")
      (st : state) (ctxt : runtime_ctxt) (e : expression) : typed_value * runtime_ctxt =
  let em, ctxt' = normalise st ctxt e in
  let (pre_result, ctxt'') = run_until_done normalise st ctxt'
                           [(work_item_name, em)] [] in
  let result =
    match pre_result with
    | [(_, result)] -> result
    | [] -> Bottom in
  evaluate_value ctxt'' result, ctxt''
