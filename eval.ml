(*
   Evaluator for Flick programs.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open General
open Debug
open Crisp_syntax
open State

let runtime_ctxt_print_indentation = "  "

(*FIXME name clash with Crisp_syntax.type_value*)
(*Representation of values during evaluation. Evaluation might take place interactively
  with the user (or with the network), so this datatype is used to ensure that only
  values (and not arbitrary expressions) are stored in variables.
  We could avoid defining the typed_value type, and working with normalised values
  (that it represents), but this way forces us to make sure we're only working
  with the intended kind of values, and not with arbitrary expressions!*)
type typed_value =
  | UserDefinedType of type_name * typed_value (*FIXME is this redundant?*)
  | String of string
  | Integer of int
  | Boolean of bool
  | RecordType of (label * typed_value) list
  | Disjoint_Union of label * typed_value
  | List of typed_value list
  | IPv4Address of int * int * int * int
  | Tuple of typed_value list
  | Dictionary of (typed_value * typed_value) list
  | Reference of typed_value
  | ChanType of channel_type
(*Channels are abstracted to behave as queues*)
and channel_type =
  | ChannelSingle of typed_value list * typed_value list
  | ChannelArray of ((*typed_value * -- FIXME currently no channel indexing*) (typed_value list * typed_value list)) list

(*FIXME include runtime_ctxt in state?*)
exception Eval_Exc of string * expression option * typed_value option (** state -- FIXME include runtime_ctxt*)

let rec string_of_list_vs vs =
  "[" ^ String.concat ", " (List.map string_of_typed_value vs) ^ "]"
and string_of_chan_vs (in_vs, out_vs) =
  "incoming:" ^ string_of_list_vs in_vs ^ " outgoing:" ^ string_of_list_vs out_vs
and string_of_typed_value : typed_value -> string = function
  | UserDefinedType (_, v) -> string_of_typed_value v
  | String s -> "\"" ^ s ^ "\""
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | RecordType fields ->
    let entry_to_string (l, v) =
      l ^ " = " ^ string_of_typed_value v in
    "{" ^
    String.concat ", " (List.map entry_to_string fields) ^ "}"
  | Disjoint_Union (l, v) -> l ^ "(" ^ string_of_typed_value v ^ ")"
  | List vs -> string_of_list_vs vs
  | IPv4Address (i1, i2, i3, i4) ->
    string_of_int i1 ^ "." ^ string_of_int i2 ^ "." ^ string_of_int i3 ^ "." ^ string_of_int i4
  | Tuple vs ->
    "<" ^ String.concat ", " (List.map string_of_typed_value vs) ^ ">"
  | Dictionary d ->
    let d_s = List.map (fun (k, v) ->
      string_of_typed_value k ^ " |-> " ^ string_of_typed_value v) d in
    "[" ^ String.concat ", " d_s ^ "]"
  | Reference v -> string_of_typed_value v
  | ChanType cv -> string_of_channel_type cv
and string_of_channel_type : channel_type -> string = function
  | ChannelSingle (in_vs, out_vs) -> string_of_chan_vs (in_vs, out_vs)
  | ChannelArray chans ->
    (*FIXME index these according to the indexing scheme chosen for this channel
            array. for the time being, indexing is implicit: i just show a list
            of array elements.*)
    "[" ^ String.concat ", " (List.map string_of_chan_vs chans) ^ "]"

(*Symbols are identifiers of values (aka variables), functions, channels, and processes.*)
type symbol_name = string

type value_table = (symbol_name * typed_value) list
type runtime_ctxt =
  {
    (*Current value held by each variable.
      NOTE we fix a single, global, scope. Might replace this with nested scopes, but this will do fine for now.*)
    value_table : value_table;
    (*Table of function and process declarations*)
    exec_table : (symbol_name * toplevel_decl) list;
    (*Stack of active exceptions*)
    except_table : excepts_decl list list;
    (*Table of function/process-state, both local and shared*)
    state: (symbol_name * value_table) list;
  }
let strlist_of_value_table =
  List.map (fun (name, v) ->
    name ^ " = " ^ string_of_typed_value v)
let string_of_runtime_ctxt ?indentation:(indentation : int = 0)
   ?indentation_step:(indentation_step : int = 2) (ctxt : runtime_ctxt) : string =
  let exec_table =
    List.map (fun (name, v) ->
      name ^ " = ..."(*FIXME currently don't show function/process code*)) ctxt.exec_table in
  let except_table =
    let scope_counts =
      enlist 0 (List.length ctxt.except_table)
      |> List.map string_of_int in
    let except_print excepts =
      List.map (fun (l, e) ->
        l ^ " = " ^ Crisp_syntax.expression_to_string 0 e) excepts in
    List.map (fun (scope_label, excs) ->
      indn indentation ^ "scope" ^ scope_label ^ " : " ^
       print_list (indn (indentation + indentation_step)) (except_print excs))
        (List.combine scope_counts ctxt.except_table) in
  let state =
    List.map (fun (scope_label, vt) ->
      indn indentation ^ "scope" ^ scope_label ^ " : " ^
       print_list (indn (indentation + indentation_step)) (strlist_of_value_table vt))
       ctxt.state in
  indn indentation ^ "value_table : " ^
    print_list (indn indentation) (strlist_of_value_table ctxt.value_table) ^ "\n" ^
  indn indentation ^ "exec_table : " ^
    print_list (indn indentation) exec_table ^ "\n" ^
  indn indentation ^ "except_table : " ^
    print_list (indn indentation) except_table ^ "\n" ^
  indn indentation ^ "state : " ^
    print_list (indn indentation) state ^ "\n"

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
    (*FIXME if state is passed to this function then could quickly check that l's
            identifier_kind is Disjunct*)
    Disjoint_Union (l, evaluate_value ctxt e)
  | _ ->
    raise (Eval_Exc ("Cannot represent as Flick expression. Perhaps it's not in normal form?", Some e, None))

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
  | Reference _ -> failwith "devaluate: TODO"
  | Dictionary _
  | ChanType _ ->
    raise (Eval_Exc ("Cannot represent as Flick expression", None, Some v))

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

(*NOTE l1 should be in normal form*)
let rec append_list (acc : expression list) (l1 : expression) (l2 : expression) : expression =
  match l1 with
  | EmptyList -> l2
  | ConsList (h, EmptyList) ->
    List.fold_right (fun x l -> ConsList (x, l)) acc (ConsList (h, l2))
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

(*Reduce an expression into a value expression*)
let rec normalise (st : state) (ctxt : runtime_ctxt) (e : expression) : expression * runtime_ctxt =
  match e with
  (*These expressions are already normal*)
  | True
  | False
  | Int _
  | IPv4_address _
  | Str _
  | EmptyList -> e, ctxt

  | Variable l ->
    begin
    match List.filter (fun (name, _) -> name = l) ctxt.value_table with
    | [] ->
      raise (Eval_Exc ("Cannot resolve variable's value", Some e, None))
    | [(_, v)] -> devaluate v, ctxt
    | results ->
      let results_s =
        strlist_of_value_table results
        |> print_list runtime_ctxt_print_indentation in
      raise (Eval_Exc ("Multiple resolusions for variable's value: " ^ results_s, Some e, None))
    end
  | TypeAnnotation (e', _) ->
    (*NOTE there's no runtime type-checking -- we ignore the type annotation.
           that should have been checked at an earlier pass.*)
    normalise st ctxt e'

  | And (e1, e2)
  | Or (e1, e2) ->
    begin
    let f =
      match e with
      | And (_, _) -> (fun b1 b2 -> if b1 = True && b2 = True then True else False)
      | Or (_, _) -> (fun b1 b2 -> if b1 = True || b2 = True then True else False)
      | _ -> failwith "Impossible" in
    let b1, b2, ctxt' =
       normalise st ctxt e1
       ||> (normalise st, e2) in
    match b1, b2 with
    | True, True
    | False, True
    | True, False
    | False, False -> f b1 b2, ctxt'
    | anomalous, True
    | anomalous, False ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to Boolean value. Got " ^ anomalous_s, Some e1, None))
    | True, anomalous
    | False, anomalous ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to Boolean value. Got " ^ anomalous_s, Some e2, None))
    | _, _ ->
      raise (Eval_Exc ("Cannot normalise to Boolean value", Some e, None))
    end
  | Not e' ->
    begin
    match normalise st ctxt e' with
    | True, ctxt' -> False, ctxt'
    | False, ctxt' -> True, ctxt'
    | _ ->
      raise (Eval_Exc ("Cannot normalise to Boolean value", Some e', None))
    end
  | Equals (e1, e2) ->
    let e1', e2', ctxt' =
       normalise st ctxt e1
       ||> (normalise st, e2) in
    let result = if e1' = e2' then True else False in
    (result, ctxt')

  | GreaterThan (e1, e2)
  | LessThan (e1, e2) ->
    begin
    let f =
      match e with
      | GreaterThan _ -> (fun i1 i2 -> if i1 > i2 then True else False)
      | LessThan _ -> (fun i1 i2 -> if i1 < i2 then True else False)
      | _ -> failwith "Impossible" in
    let e1', e2', ctxt' =
       normalise st ctxt e1
       ||> (normalise st, e2) in
    match e1', e2' with
    | Int i1, Int i2 -> f i1 i2, ctxt'
    | anomalous, Int _ ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e1, None))
    | Int _, anomalous ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e2, None))
    | _, _->
      raise (Eval_Exc ("Cannot normalise to integer value", Some e, None))
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
      | Mod _
      | Quotient _ -> failwith "TODO"
      | IntegerRange _ -> (fun i1 i2 ->
          enlist i1 i2
          |> Crisp_syntax_aux.flick_integer_list)
      | _ -> failwith "Impossible" in
    let e1', e2', ctxt' =
       normalise st ctxt e1
       ||> (normalise st, e2) in
    match e1', e2' with
    | Int i1, Int i2 -> f i1 i2, ctxt'
    | anomalous, Int _ ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e1, None))
    | Int _, anomalous ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e2, None))
    | _, _->
      raise (Eval_Exc ("Cannot normalise to integer value", Some e, None))
    end

  | Abs e' ->
    begin
    match normalise st ctxt e' with
    | Int i, ctxt' -> Int (abs i), ctxt'
    | anomalous, _ ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e', None))
    end

  | Int_to_address e' ->
    begin
    match normalise st ctxt e' with
    | Int i, _ -> failwith "TODO"
    | anomalous, _ ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e', None))
    end
  | Address_to_int e' ->
    begin
    match normalise st ctxt e' with
    | IPv4_address (i1, i2, i3, i4), _ -> failwith "TODO"
    | anomalous, _ ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to integer value. Got " ^ anomalous_s, Some e', None))
    end

  | ConsList (h, t) ->
    let h', t', ctxt' =
       normalise st ctxt h
       ||> (normalise st, t) in
    ConsList (h', t'), ctxt'
  | AppendList (l1, l2) ->
    let l1', l2', ctxt' =
       normalise st ctxt l1
       ||> (normalise st, l2) in
    append_list [] l1' l2', ctxt'

  | TupleValue es ->
    let es', ctxt' = fold_map ([], ctxt) (normalise st) es in
    TupleValue es', ctxt'

  | ITE (b, e1, e2_opt) ->
    begin
    match normalise st ctxt b with
    | True, ctxt' -> normalise st ctxt' e1
    | False, ctxt' ->
      begin
      match e2_opt with
      | None -> Crisp_syntax.flick_unit_value, ctxt'
      | Some e2 -> normalise st ctxt' e2
      end
    | anomalous, _ ->
      let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
      raise (Eval_Exc ("Cannot normalise to Boolean value. Got " ^ anomalous_s, Some b, None))
    end

  | Record fields ->
    let fields', ctxt' =
      fold_map ([], ctxt) (fun ctxt (l, e) ->
        let e', ctxt' = normalise st ctxt e in
        ((l, e'), ctxt')) fields in
    Record fields', ctxt'

  | CaseOf (e', cases) ->
    begin
    let e'norm, ctxt' = normalise st ctxt e' in
    let disj, (arg, ctxt'') =
      match e'norm with
      | Functor_App (f_name, [Exp e]) ->
        (*NOTE would be prudent to check if the functor is indeed a disjunct*)
        (f_name, normalise st ctxt' e)
      | anomalous ->
        let anomalous_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation anomalous in
        raise (Eval_Exc ("Cannot normalise to disjunct. Got " ^ anomalous_s, Some e', None)) in
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
      raise (Eval_Exc ("Found multiple cases for this normal disjunct: " ^ e'_s, Some e'norm, None))
    end

  (*This work for both tuples and records.*)
  | RecordProjection (e', l) ->
    begin
    let e'norm, ctxt' = normalise st ctxt e' in
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
        enlist 1 (List.length es)
        |> List.map string_of_int in
      List.combine labels es
      |> project_from
    | _ ->
      raise (Eval_Exc ("Cannot project from this normal expression: " ^ e'norm_s, Some e, None))
    end

  | RecordUpdate (record_e, (field_name, field_body_e)) ->
    begin
    let e'norm, ctxt' = normalise st ctxt record_e in
    let e'norm_s = Crisp_syntax.expression_to_string Crisp_syntax.min_indentation e'norm in
    match e'norm with
    | Record fields ->
      let updated, fields', ctxt'' =
        List.fold_right (fun ((field_l, field_e) as field) (updated, field_acc, ctxt) ->
          if field_l = field_name then
            if updated then
              raise (Eval_Exc ("Cannot record-update this normal expression: " ^ e'norm_s, Some e, None))
            else
              let field_body_e', ctxt' = normalise st ctxt field_body_e in
              (true, (field_name, field_body_e') :: field_acc, ctxt')
          else (updated, field :: field_acc, ctxt)) (List.rev fields) (false, [], ctxt') in
      if updated then
        Record fields', ctxt''
      else
        raise (Eval_Exc ("Could not find field name " ^ field_name ^ " to update in this normal expression: " ^ e'norm_s, Some e, None))
    | _ ->
      raise (Eval_Exc ("Cannot record-update this normal expression: " ^ e'norm_s, Some e, None))
    end

  | Map (v, l, body, unordered) ->
    (*FIXME "unordered" not taken into account*)
    let l', ctxt' = normalise st ctxt l in
    let l' = interpret_flick_list l' in
    let l'', ctxt'' =
      fold_map ([], ctxt') (fun ctxt e ->
        Crisp_syntax_aux.subst_var v e body
        |> normalise st ctxt) l' in
    Crisp_syntax_aux.flick_list l'', ctxt''

  | Iterate (v, l, acc_opt, body, unordered) ->
    (*FIXME "unordered" not taken into account*)
    begin
    let l', ctxt' = normalise st ctxt l in
    let l' = interpret_flick_list l' in
    let acc_opt', ctxt'' =
      match acc_opt with
      | None -> None, ctxt'
      | Some (l, e) ->
        let e', ctxt'' = normalise st ctxt e in
        Some (l, e'), ctxt'' in
    let result, ctxt''' =
      List.fold_right (fun e (acc_opt, ctxt) ->
        match acc_opt with
        | None ->
          let _, ctxt' =
            Crisp_syntax_aux.subst_var v e body
            |> normalise st ctxt in
          None, ctxt'
        | Some (acc_l, acc_e) ->
          let acc', ctxt' =
            Crisp_syntax_aux.subst_var v e body
            |> Crisp_syntax_aux.subst_var acc_l acc_e
            |> normalise st ctxt in
          Some (acc_l, acc'), ctxt') l' (acc_opt', ctxt'') in
    match result with
    | None -> flick_unit_value, ctxt'''
    | Some (_, e) -> e, ctxt'''
    end

  | Functor_App (function_name, fun_args) ->
    begin
    (*Determine whether this is a function call, or disjunct.
      If the latter, then it's already a value -- cannot be reduced further.*)
    match lookup_term_data (Term Undetermined) st.term_symbols function_name with
    | None ->
      raise (Eval_Exc ("Could not retrieve metadata from symbol table, for functor:" ^ function_name, Some e, None))
    | Some (_, {identifier_kind; _}) ->
      begin
      match identifier_kind with
      | Disjunct _ -> e, ctxt
      | Function_Name ->
        begin
        let normal_arg_es, ctxt' =
          (*normalise each actual parameter*)
          fold_map ([], ctxt) (fun ctxt arg ->
            match arg with
            | Exp e -> normalise st ctxt e
            | Named _ -> failwith "TODO") fun_args in
        let (stata, body, excs) =
          (*Get function implementation*)
          match List.filter (fun (name, _) -> name = function_name) ctxt.exec_table with
          | [] ->
            raise (Eval_Exc ("Could not retrieve implementation from runtime context, for functor:" ^ function_name, Some e, None))
          | [(_, Function {fn_body; _})] ->
            begin
            match fn_body with
            | ProcessBody (stata, body, excs) -> (stata, body, excs)
            end
          | [(_, Process {process_body; _})] ->
            raise (Eval_Exc ("Calling processes not supported, for functor:" ^ function_name, Some e, None))
          | _ ->
            raise (Eval_Exc ("Invalid declaration found when calling functor:" ^ function_name, Some e, None)) in
        let ((chans, arg_tys), ret_tys) =
          match lookup_function_type st function_name with
          | None ->
            raise (Eval_Exc ("Could not retrieve type from symbol table, for functor:" ^ function_name, Some e, None))
          | Some ft -> Crisp_syntax_aux.extract_function_types ft in
        assert (chans = []);
        let formal_arg_names =
          List.map (fun ty ->
            match Crisp_syntax_aux.label_of_type ty with
            | None ->
              let ty_s = type_value_to_string true false min_indentation ty in
              raise (Eval_Exc ("Missing label for parameter typed " ^ ty_s ^ " for functor " ^ function_name, Some e, None))
            | Some label -> label) arg_tys in

        (*FIXME currently the function set-up and calling isn't implemented properly.
            What remains to be done:
            - set up handlers for exceptions, and "connect" function's state
            - after the function body has been normalised, detach state, and unwind exception handlers.
        *)
        List.fold_right (fun (v, arg) body ->
          (*Substitute formal for actual parameters in the function body*)
          Crisp_syntax_aux.subst_var v arg body)
         (List.combine formal_arg_names normal_arg_es) body
        (*Normalise the function body*)
        |> normalise st ctxt'
        end
      | _ ->
        raise (Eval_Exc ("Functor " ^ function_name ^ " had inconsistent identifier kind " ^
               string_of_identifier_kind identifier_kind, Some e, None))
      end
    end

  | LocalDef ((v, _), e) ->
    begin
    let e', ctxt' = normalise st ctxt e in
    let ctxt'' = { ctxt' with
      (*NOTE we don't evict previous mappins of v*)
      value_table = (v, evaluate_value ctxt' e') :: ctxt'.value_table } in
    e', ctxt''
    end

(*
  | Update of value_name * expression (*value_name := expression*)
  (*value_name[idx] := expression*)
  | UpdateIndexable of value_name * expression * expression

  | IndexableProjection of label * expression

  | Send of expression * expression
  | Receive of expression * expression
  | Exchange of expression * expression
*)

  | Seq (Meta_quoted mis, e') ->
    (*FIXME currently ignoring meta-quoted instructions*)
    normalise st ctxt e'
  | Meta_quoted _ ->
    raise (Eval_Exc ("Cannot normalise meta_quoted expressions alone -- add some other expression after them, and normalisation should succeed.", Some e, None))

  | Seq (e1, e2) ->
    let _, ctxt' = normalise st ctxt e1 in
    normalise st ctxt' e2

  | Hole -> raise (Eval_Exc ("Cannot normalise", Some e, None))

(*Translate an arbitrary expression into a value*)
let evaluate (st : state) (ctxt : runtime_ctxt) (e : expression) : typed_value =
  normalise st ctxt e
  |> swap
  |> uncurry evaluate_value
