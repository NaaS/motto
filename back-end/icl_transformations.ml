(*
   Program transformations related to the ICL runtime.
   Nik Sultana, Cambridge University Computer Lab, October 2015
*)


open General
open State
open Naasty
open Crisp_syntax

let rec preprocess_exp st e : Crisp_syntax.expression * State.state =
  match e with
  | Send (false, dest, Receive (false, chan)) ->
    (*This is an abbreviation for "forwarding" a value from one channel to
       another*)
    (*FIXME would be neater to factor out this transformation into a
            separate function that's called before calling the translation
            (on the transformed program).*)

    let i = string_of_int st.next_symbol in
    let name = "_forward_idx_" ^ i in
(*
    FIXME the above 2 lines work, but aren't very clean. It would be nicer to
          set things up so that the freshness of names is managed through the
          usual (same) mechanism; something like the following:
    let i, st =
    (string_of_int st.next_symbol,
     st (*{ st with next_symbol = 1 + st.next_symbol }*)) in
    let (name, _, st) =
      mk_fresh (Term Value)
        ~src_ty_opt:(Some (Integer (None, [])(*FIXME use type inference*)))
        ~ty_opt:(Some (Int_Type (None, default_int_metadata)(*FIXME use type inference*)))
        ("forward_idx_" ^ i ^ "_") 0 st in
*)
    let e' =
      (*This is the idiom used to forward between channels
        in the ICL backend, since the interpretation of "Receive"
        side-effects and removes the value.*)
      Crisp_syntax.Seq (LocalDef ((name, None), Peek (false, chan)),
           Seq (Send (false, dest, Variable name),
                Receive (false, chan))) in
    let st' = { st with next_symbol = st.next_symbol + 1 } in
    e', st'

  | Can e ->
    let e', st' = preprocess_exp st e in
    Can e', st'
  | Bottom
  | InvertedVariable _
  | Variable _ -> e, st
  | TypeAnnotation (e, ty) ->
    let e', st' = preprocess_exp st e in
    TypeAnnotation (e', ty), st'

  | True
  | False -> e, st

  | And (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    And (e1', e2'), st''
  | Or (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Or (e1', e2'), st''
  | Not e ->
    let e', st' = preprocess_exp st e in
    Not e', st'

  | Equals (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Equals (e1', e2'), st''
  | GreaterThan (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    GreaterThan (e1', e2'), st''
  | LessThan (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    LessThan (e1', e2'), st''

  | Int _ -> e, st
  | Plus (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Plus (e1', e2'), st''
  | Minus (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Minus (e1', e2'), st''
  | Times (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Times (e1', e2'), st''
  | Mod (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Mod (e1', e2'), st''
  | Quotient (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Quotient (e1', e2'), st''
  | Abs e ->
    let e', st' = preprocess_exp st e in
    Abs e', st'

  | IPv4_address _ -> e, st
  | Int_to_address e ->
    let e', st' = preprocess_exp st e in
    Int_to_address e', st'
  | Address_to_int e ->
    let e', st' = preprocess_exp st e in
    Address_to_int e', st'

  | EmptyList -> e, st
  | ConsList (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    ConsList (e1', e2'), st''
  | AppendList (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    AppendList (e1', e2'), st''

  | TupleValue es ->
    let es', st' =
      fold_map ([], st) preprocess_exp es in
    TupleValue es', st'

  | Seq (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Seq (e1', e2'), st''
  | Par (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Par (e1', e2'), st''
  | ITE (e1, e2, e3_opt) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    let e3_opt', st''' =
      bind_opt (fun e3 ->
        let e3', st''' = preprocess_exp st'' e3 in
        Some e3', st''') (None, st'') e3_opt in
    ITE (e1', e2', e3_opt'), st'''
  | LocalDef (ty, e) ->
    let e', st' = preprocess_exp st e in
    LocalDef (ty, e'), st'
  | Update (name, e) ->
    let e', st' = preprocess_exp st e in
    Update (name, e'), st'
  | UpdateIndexable (name, e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    UpdateIndexable (name, e1', e2'), st''

  | RecordProjection (e, label) ->
    let e', st' = preprocess_exp st e in
    RecordProjection (e', label), st'

  | Functor_App (function_name, fun_args) ->
    let fun_args', st' =
      fold_map ([], st) (fun st fun_arg ->
        match fun_arg with
        | Exp e ->
          let e', st' = preprocess_exp st e in
          Exp e', st'
        | Named (label, e) ->
          let e', st' = preprocess_exp st e in
          Named (label, e'), st') fun_args in
    Functor_App (function_name, fun_args'), st'

  | Record fields ->
    let fields', st' =
      fold_map ([], st) (fun st (label, e) ->
        let e', st' = preprocess_exp st e in
        (label, e'), st') fields in
    Record fields', st'

  | RecordUpdate (e1, (label, e2)) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    RecordUpdate (e1', (label, e2')), st''

  | CaseOf (e, cases) ->
    let e', st' = preprocess_exp st e in
    let cases', st'' =
      fold_map ([], st') (fun st (e1, e2) ->
        let e1', st' = preprocess_exp st e1 in
        let e2', st'' = preprocess_exp st' e2 in
        (e1', e2'), st'') cases in
    CaseOf (e', cases'), st''

  | IndexableProjection (label, e) ->
    let e', st' = preprocess_exp st e in
    IndexableProjection (label, e'), st'

  | IntegerRange (e1, e2) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    IntegerRange (e1', e2'), st''
  | Map (label, e1, e2, b) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    Map (label, e1', e2', b), st''
  | Iterate (label, e1, acc_opt, e2, b) ->
    let e1', st' = preprocess_exp st e1 in
    let e2', st'' = preprocess_exp st' e2 in
    let acc_opt', st''' =
      bind_opt (fun (label, e) ->
        let e', st''' = preprocess_exp st'' e in
        Some (label, e'), st''') (None, st'') acc_opt in
    Iterate (label, e1', acc_opt', e2', b), st'''

  | Send (c_inv, ci, e) ->
    let e', st' = preprocess_exp st e in
    Send (c_inv, ci, e'), st'
  | Receive _
  | Peek _ -> e, st

  | Str _
  | Meta_quoted _
  | Hole -> e, st

let rec preprocess_decl st toplevel_decl =
  match toplevel_decl with
  | Type _ -> toplevel_decl, st
  | Function {fn_name; fn_params; fn_body} ->
    let fn_body', st' =
      match fn_body with
      | ProcessBody (state_decls, body, excepts_decls) ->
        let body', st' = preprocess_exp st body in
        (*NOTE currently we don't recurse into expressions within state_decls
               and excepts_decls*)
        ProcessBody (state_decls, body', excepts_decls), st' in
      Function {fn_name = fn_name; fn_params = fn_params; fn_body = fn_body'}, st'
  | Process {process_name; process_type; process_body} ->
    (*We rewrite a process declaration into a specific type of function
      declaration*)
    let fn_params =
      match process_type with
      | ProcessType (dis, (chans, args)) ->
        FunType (dis, FunDomType (chans, args)(*the function signature is
                                                transformed during post-processing*),
                 FunRetType [IL_Type Types.task_event_ty]) in
    let fun_decl =
      Function
        {fn_name = process_name;
         fn_params = fn_params;
         fn_body = process_body}
    in
    preprocess_decl st fun_decl

  | Include _ -> toplevel_decl, st

(*If thy body of a function returning TaskEvent never assigns to the variable
  carrying its return value, then assign "TaskEvent(TaskEvent::OK)" to that
  variable.*)
let ensure_result_is_assigned_to (result_idx : identifier)
      (body : naasty_statement) : naasty_statement =
  let no_assignments = Naasty_aux.count_toplevel_assignments result_idx body 0 in
  assert (no_assignments >= 0);
  if !Config.cfg.Config.verbosity > 1 then
    print_endline ("ensure_result_is_assigned_to: " ^
                   string_of_int no_assignments ^ " assignments to result_idx");
  if no_assignments > 0 then
    (*We do assign to the result_idx, so do nothing here*)
    body
  else
    match Naasty_aux.all_but_last_return_expression Skip body with
    | None -> failwith "Expecting a 'return' statement in body"
    | Some body ->
      Return (Some (Var result_idx))
      |> Naasty_aux.mk_seq
           (Assign (Var result_idx, Literal "TaskEvent(TaskEvent::OK)"))
      |> Naasty_aux.mk_seq body
