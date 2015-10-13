(*
   Analysis and implementation of an inliner transformation on NaaSty code.
   Nik Sultana, Cambridge University Computer Lab, May 2015

   NOTE some definitions in this module, such as free_vars, are general enough
   to be moved to Naasty_aux.
*)

open General
open Crisp_syntax
open Naasty
open State
open Naasty_aux


exception Inliner_Exc of string * state option * naasty_statement option

(*
  Table of identifiers, the number of times they are assigned to in this
  scope and child scopes, and, if the value of that field is 1, then we could
   also have the value they could be replaced with*)
type inliner_table_entry =
  {
    id : identifier;
    parameter : bool;
    update_count : int;
    ref_count : int;
    initialisation : naasty_expression option;
    assignment : naasty_expression option;
  }

(*Initialise the inliner table, and mention the parameters in the initial table*)
let init_table = List.map (fun idx ->
  {
    id = idx;
    parameter = true;
    update_count = 0;
    ref_count = 0;
    initialisation = None;
    assignment = None;
  })

let rec count_var_references_in_naasty_expr (st : state)
  (expr : naasty_expression) (table : inliner_table_entry list) : inliner_table_entry list =
  match expr with
  | Var idx ->
    let no_idx_entries =
      List.fold_right (fun (entry : inliner_table_entry) acc ->
        if entry.id = idx then acc + 1 else acc) table 0 in
    assert (no_idx_entries >= 0);
    if no_idx_entries = 1 then
      (*Increment times we've referenced this idx*)
      List.map (fun (entry : inliner_table_entry) ->
        if entry.id = idx then
          { entry with ref_count = entry.ref_count + 1 }
        else entry) table
    else
      (*It's OK for something to have 0 references, as long as it's of
        the right identifier_kind: Field, say.*)
      let label = resolve_idx (Term Undetermined) no_prefix (Some st) idx in
      let md =
        match lookup_term_data (Term Undetermined) st.term_symbols label with
        | None -> failwith "Impossible"
        | Some (_, md) -> md in
      if ik_is_field md.identifier_kind then table
      else
        raise (Inliner_Exc ("Undeclared variable: " ^
                            string_of_int no_idx_entries ^
                            " records for the idx " ^
                            string_of_int idx ^ " -- variable '" ^ label ^ "'",
                            Some st, Some (St_of_E expr)))

  | Int_Value _
  | Nullptr
  | Bool_Value _
  | Literal _
  | Const _ ->
    table
  | Not e
  | Abs e
  | Dereference e
  | Address_of e
  | Cast (_, e) ->
    count_var_references_in_naasty_expr st e table
  | And (e1, e2)
  | Or (e1, e2)
  | Plus (e1, e2)
  | Equals (e1, e2)
  | Lt (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | GEq (e1, e2)
  | Gt (e1, e2)
  | Field_In_Record (e1, e2)
  | LEq (e1, e2)
  | ArrayElement (e1, e2) ->
    count_var_references_in_naasty_expr st e1 table
    |> count_var_references_in_naasty_expr st e2
  | Call_Function (_ (*ignore function identifier*), tps, exprs) ->
    List.fold_right (fun expr acc ->
      count_var_references_in_naasty_expr st expr acc) exprs table
    |> List.fold_right (fun tp acc ->
        match tp with
        | Type_Parameter _ -> acc
        | Term_Parameter e ->
          count_var_references_in_naasty_expr st e acc) tps
  | Record_Value fields ->
    List.fold_right (fun (_, e) table_acc ->
      count_var_references_in_naasty_expr st e table_acc) fields table

let rec count_var_references_in_naasty_stmt (st : state)
  (stmt : naasty_statement) (table : inliner_table_entry list) : inliner_table_entry list =
  match stmt with
  | Declaration (_, expr_opt) ->
    begin
    match expr_opt with
    | None -> table
    | Some expr -> count_var_references_in_naasty_expr st expr table
    end
  | Seq (stmt1, stmt2) ->
    count_var_references_in_naasty_stmt st stmt1 table
    |> count_var_references_in_naasty_stmt st stmt2
  | Assign (_ (*ignore lvalue*), expr)
  | Increment (_, expr)
  | St_of_E expr ->
    count_var_references_in_naasty_expr st expr table
  | For ((_, expr, stmt), body) ->
    count_var_references_in_naasty_expr st expr table
    |> count_var_references_in_naasty_stmt st stmt
    |> count_var_references_in_naasty_stmt st body
  | If (be, e1, e2) ->
    count_var_references_in_naasty_expr st be table
    |> count_var_references_in_naasty_stmt st e1
    |> count_var_references_in_naasty_stmt st e2
  | If1 (be, e1) ->
    count_var_references_in_naasty_expr st be table
    |> count_var_references_in_naasty_stmt st e1
  | Return expr_opt ->
    begin
      match expr_opt with
      | None -> table
      | Some expr -> count_var_references_in_naasty_expr st expr table
    end
  | Break
  | Continue
  | Skip -> table
  | Commented (stmt, _) -> count_var_references_in_naasty_stmt st stmt table
  | Switch (e, cases) ->
    List.fold_right (fun (e, stmt) table ->
      count_var_references_in_naasty_expr st e table
      |> count_var_references_in_naasty_stmt st stmt)
      cases (count_var_references_in_naasty_expr st e table)

let inliner_table_entry_to_string ?st_opt:((st_opt : state option) = None)
      (entry : inliner_table_entry) =
  "id=" ^ string_of_int entry.id ^
  bind_opt (fun st ->
    " (" ^ resolve_idx (Term Undetermined) no_prefix st_opt entry.id ^ ")") "" st_opt ^ "; " ^
  "parameter=" ^ string_of_bool entry.parameter ^ "; " ^
  "update_count=" ^ string_of_int entry.update_count ^ "; " ^
  "ref_count=" ^ string_of_int entry.ref_count ^ "; " ^
  "initialisation=" ^
  bind_opt (fun e -> fst (string_of_naasty_expression ~st_opt e)) "<None>"
    entry.initialisation ^ "; " ^
  "assignment=" ^
  bind_opt (fun e -> fst (string_of_naasty_expression ~st_opt e)) "<None>"
    entry.assignment

(*Removes redundant definitions that are introduced in a profligate manner by
  naasty_of_flick_expr*)
(*For analysis just mutate the inliner_table_entry*)
let rec inliner_analysis (st : state) (stmt : naasty_statement)
  (ctxt_acc : identifier list)
  (table : inliner_table_entry list) : inliner_table_entry list =
  match stmt with
  | Declaration (ty, expr_opt) ->
    begin
    match idx_of_naasty_type ty with
    | None ->
      raise (Inliner_Exc ("Declaration must contain an identifier name, not just mention a type!" ,
                          Some st, Some stmt))
    | Some idx ->
      let no_idx_entries =
        List.fold_right (fun (entry : inliner_table_entry) acc ->
          if entry.id = idx then acc + 1 else acc) table 0 in
      assert (no_idx_entries >= 0);
      if no_idx_entries = 0 then
        {
          id = idx;
          parameter = false;
          update_count = if expr_opt = None then 0 else 1;
          ref_count = 0;
          initialisation = expr_opt;
          assignment = None;
        } :: table
      else if no_idx_entries = 1 then
        (*Increment times we've assigned to this idx*)
        List.map (fun (entry : inliner_table_entry) ->
          if entry.id = idx then
            if entry.initialisation <> None then
              raise (Inliner_Exc ("Variable has already been initialised",
                                  Some st, Some stmt))
            else
              { entry with update_count = entry.update_count + 1;
                           initialisation = expr_opt }
          else entry) table
      else
        raise (Inliner_Exc ("Impossible: multiple records for the same idx",
                            Some st, Some stmt))
    end

  | Assign (Var idx, expr) ->
    let no_idx_entries =
      List.fold_right (fun (entry : inliner_table_entry) acc ->
        if entry.id = idx then acc + 1 else acc) table 0 in
    assert (no_idx_entries >= 0);
    if no_idx_entries = 1 then
      (*Increment times we've assigned to this idx*)
      List.map (fun (entry : inliner_table_entry) ->
        if entry.id = idx then
          if entry.initialisation <> None then
            raise (Inliner_Exc ("Variable has already been initialised",
                                Some st, Some stmt))
          else
            { entry with update_count = entry.update_count + 1;
                         assignment = Some expr }
        else entry) table
    else
      raise (Inliner_Exc ("Impossible: " ^ string_of_int no_idx_entries ^
                " records for the idx " ^
                string_of_int idx ^ " -- variable " ^
                resolve_idx (Term Value) no_prefix (Some st) idx,
               Some st, Some stmt))

  | Assign (_, _) ->
    (*The lvalue is not a Var (because we've already checked that), so this
      lvalue isn't a temporary variable that we generated. So we don't bother
      trying to inline it.*)
    table

  | Increment (idx, expr) ->
    let no_idx_entries =
      List.fold_right (fun (entry : inliner_table_entry) acc ->
        if entry.id = idx then acc + 1 else acc) table 0 in
    assert (no_idx_entries >= 0);
    (*We MUST have at least initialised this variable*)
    if no_idx_entries = 0 then
      raise (Inliner_Exc ("Uninitialised variable", Some st, Some stmt))
    else if no_idx_entries = 1 then
      (*Increment times we've assigned to this idx*)
      List.map (fun (entry : inliner_table_entry) ->
        if entry.id = idx then
          if entry.initialisation <> None then
            raise (Inliner_Exc ("Variable has already been initialised",
                                Some st, Some stmt))
          else
            { entry with update_count = entry.update_count + 1;
                         assignment = Some (Plus (Var idx, expr)) }
        else entry) table
    else
      raise (Inliner_Exc ("Impossible: multiple records for the same idx " ^
                string_of_int idx ^ " -- variable " ^
                resolve_idx (Term Value) no_prefix (Some st) idx,
              Some st, Some stmt))

  | Commented (stmt', _) ->
    inliner_analysis st stmt' ctxt_acc table

  | For ((cursor, test, update), body) ->
    inliner_analysis st body ctxt_acc table

  | If (cond, then_body, else_body) ->
    inliner_analysis st then_body ctxt_acc table
    |> inliner_analysis st else_body ctxt_acc

  | If1 (cond, body) ->
    inliner_analysis st body ctxt_acc table

  | Seq (stmt1, stmt2) ->
    inliner_analysis st stmt1 ctxt_acc table
    |> inliner_analysis st stmt2 ctxt_acc

  | Switch (_, cases) ->
    List.fold_right (fun (_, stmt) table ->
      inliner_analysis st stmt ctxt_acc table) cases table

  | Label (_, stmt') ->
    inliner_analysis st stmt' ctxt_acc table

  (*FIXME the next 3 phrases include statements/expressions, should we include
          them in the analysis?*)
  | St_of_E _
  | Return _

  | Skip
  | Break
  | Continue
  | GotoLabel _ -> table

(*
   Remove entries where update_count <> 1 and ref_count <> 1,
   find those where assignment is some Var, and inline those into other
   expressions in the table. add these to the deletion line

   compute substitutions for variables
   and deletions of assignments and declarations
*)
let variables_to_be_inlined : inliner_table_entry list -> inliner_table_entry list =
  List.filter (fun entry ->
    entry.parameter = false &&
    entry.update_count = 1 &&
    entry.ref_count = 1)

(*Pick out any variables that are never read; all their assignments and their
  declaration can be erased.*)
let variables_to_be_erased : inliner_table_entry list -> inliner_table_entry list =
  List.filter (fun entry ->
    entry.parameter = false &&
    entry.ref_count = 0)

let rec naasty_expression_weight = function
  | Int_Value _
  | Bool_Value _ -> 0
  | Var _
  | Const _ -> 1
  | Abs e
  | Not e
  | Dereference e
  | Cast (_, e)
  | Address_of e -> naasty_expression_weight e + 1
  | And (e1, e2)
  | Or (e1, e2)
  | Plus (e1, e2)
  | Equals (e1, e2)
  | Lt (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | GEq (e1, e2)
  | Gt (e1, e2)
  | Field_In_Record (e1, e2)
  | LEq (e1, e2)
  | ArrayElement (e1, e2) ->
    naasty_expression_weight e1 +
    naasty_expression_weight e2 + 1
  | Call_Function (_, tps, exprs) ->
    List.fold_right (fun expr acc -> acc + naasty_expression_weight expr) exprs 1
    |> List.fold_right (fun tp acc ->
      acc +
      match tp with
      | Type_Parameter _ -> acc
      | Term_Parameter expr -> naasty_expression_weight expr) tps
  | Record_Value fields ->
    List.fold_right (fun (_, e) weight_acc ->
      naasty_expression_weight e + weight_acc) fields 1
  | e ->
    raise (Inliner_Exc ("naasty_expression_weight", None, Some (St_of_E e)))

let inliner_table_entry_weight entry =
  bind_opt naasty_expression_weight 0 entry.initialisation +
  bind_opt naasty_expression_weight 0 entry.assignment

let inliner_table_entry_order entry1 entry2 =
  let entry1_weight = inliner_table_entry_weight entry1 in
  let entry2_weight = inliner_table_entry_weight entry2 in
  if entry1_weight = entry2_weight then 0
  else if entry1_weight > entry2_weight then 1
  else
    begin
      assert (entry1_weight < entry2_weight);
      -1
    end

type substitution = (identifier * naasty_expression) list

module Identifier_Set = Set.Make(
 struct
   type t = identifier
     (*FIXME unsure if i should use "compare" from Pervasives, since using
       polymorphic compare seems bad, unless the compiler inlines a better
       definition.*)
   let compare x y =
     if x = y then 0
     else if x < y then -1
     else 1
 end)

let rec free_vars (expr : naasty_expression) (acc : Identifier_Set.t) : Identifier_Set.t =
  match expr with
  | Var id -> Identifier_Set.add id acc

  | Int_Value _
  | Bool_Value _
  | Const _ -> acc

  | Cast
      (_ (*we don't count any identifiers used in types*),
       e) ->
    free_vars e acc

  | Call_Function
      (_ (*we don't count the function name as a free variable*),
       tps,
       exprs) ->
    List.fold_right free_vars exprs acc
    |> List.fold_right (fun tp acc ->
        match tp with
        | Type_Parameter _ -> acc
        | Term_Parameter e -> free_vars e acc) tps

  | Not e
  | Address_of e
  | Dereference e
  | Abs e -> free_vars e acc

  | And (e1, e2)
  | Or (e1, e2)
  | Plus (e1, e2)
  | Equals (e1, e2)
  | Lt (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | GEq (e1, e2)
  | Gt (e1, e2)
  | Field_In_Record (e1, e2)
  | LEq (e1, e2)
  | ArrayElement (e1, e2) ->
    List.fold_right free_vars [e1; e2] acc
  | Record_Value fields ->
    List.fold_right (fun (_, e) acc ->
      free_vars e acc) fields acc

(*Generate substitution from inliner_table_entry.
  NOTE we carry out an occurs-check to avoid an infinite loop due
       to replacing a variable with an expression containing that
       variable, and then replacing that variable with...*)
let inliner_to_subst (table : inliner_table_entry list) : substitution =
  let rec inliner_to_subst' (table : inliner_table_entry list) (acc : substitution) : substitution =
  match table with
  | [] -> acc
  | (entry :: rest) ->
    (*NOTE here could assert that update_count and ref_count are both =1*)
    if List.mem_assoc entry.id acc then
      raise (Inliner_Exc ("Substitution variable appears more than once in the substitution",
                          None, None))
    else if entry.assignment = None then
      raise (Inliner_Exc ("Nothing to substitute", None, None))
    else
      let assignment = the entry.assignment in
      let occurs_check =
        free_vars assignment Identifier_Set.empty
        |> Identifier_Set.exists (fun elt -> elt = entry.id) in
      if occurs_check then
        (*NOTE we silently avoid the substitution, since it would result
               in a non-terminating computation downstream.*)
        inliner_to_subst' rest acc
      else
        inliner_to_subst' rest ((entry.id, the entry.assignment) :: acc)
  in inliner_to_subst' table []

let rec subst_expr (subst : substitution) (expr : naasty_expression) : naasty_expression =
  let unary_op_inst e f = f (subst_expr subst e) in
  let binary_op_inst e1 e2 f =
    f (subst_expr subst e1) (subst_expr subst e2)
  in match expr with
  | Var id ->
    if List.mem_assoc id subst then
      List.assoc id subst
    else expr
  | Int_Value _
  | Nullptr
  | Bool_Value _
  | Literal _
  | Const _ -> expr
  | Cast (ty, e) -> unary_op_inst e (fun e' -> Cast (ty, e'))
  | Not e -> unary_op_inst e (fun e' -> Not e')
  | Abs e -> unary_op_inst e (fun e' -> Abs e')
  | And (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> And (e1', e2'))
  | Or (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Or (e1', e2'))
  | Plus (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Plus (e1', e2'))
  | Equals (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Equals (e1', e2'))
  | Lt (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Lt (e1', e2'))
  | Minus (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Minus (e1', e2'))
  | Times (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Times (e1', e2'))
  | Mod (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Mod (e1', e2'))
  | Quotient (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Quotient (e1', e2'))
  | Call_Function (id, tps, es) ->
    let es' = List.map (subst_expr subst) es in
    let tps' = List.map (fun tp ->
      match tp with
      | Type_Parameter _ -> tp
      | Term_Parameter e -> Term_Parameter (subst_expr subst e)) tps in
    Call_Function (id, tps', es')
  | GEq (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> GEq (e1', e2'))
  | Gt (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Gt (e1', e2'))
  | Dereference e -> unary_op_inst e (fun e' -> Dereference e')
  | Field_In_Record (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> Field_In_Record (e1', e2'))
  | Address_of e -> unary_op_inst e (fun e' -> Address_of e')
  | LEq (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> LEq (e1', e2'))
  | Record_Value fields ->
    let fields' = List.map (fun (i, e) ->
      (i, subst_expr subst e)) fields in
    Record_Value fields'
  | ArrayElement (e1, e2) -> binary_op_inst e1 e2 (fun e1' e2' -> ArrayElement (e1', e2'))

(*FIXME not sure there's any good reason why subst_assignee should be set to
  "false"*)
let rec subst_stmt ?subst_assignee:((subst_assignee : bool) = true)
          (subst : substitution) (stmt : naasty_statement) : naasty_statement =
  match stmt with
  | Skip
  | Break
  | Continue -> stmt
  | St_of_E e -> St_of_E (subst_expr subst e)
  | Increment (id, e) ->
    Increment (id (*we don't substitute into the id, since we cannot map it into
                  an expression -- unlike Assign statements.*),
               subst_expr subst e)
  | Commented (stmt', str) ->
    Commented (subst_stmt subst stmt', str)
  | Declaration (ty, e_opt) ->
    Declaration (ty (*we don't substitute into the type, we don't expect to find
                      expressions there*),
                 bind_opt (fun e -> Some (subst_expr subst e)) None e_opt)
  | Seq (stmt1, stmt2) ->
    Seq (subst_stmt subst stmt1, subst_stmt subst stmt2)
  | Assign (e1, e2) ->
    let e1' =
      if subst_assignee then
        subst_expr subst e1
      else e1 in
    Assign (e1', subst_expr subst e2)
  | For ((ty, cond, inc), body) ->
    let cond' = subst_expr subst cond in
    let inc' = subst_stmt subst inc in
    let body' = subst_stmt subst body in
    For ((ty, cond', inc'), body')
  | If (cond, stmt1, stmt2) ->
    let cond' = subst_expr subst cond in
    let stmt1' = subst_stmt subst stmt1 in
    let stmt2' = subst_stmt subst stmt2 in
    If (cond', stmt1', stmt2')
  | If1 (cond, stmt') ->
    let cond' = subst_expr subst cond in
    let stmt'' = subst_stmt subst stmt' in
    If1 (cond', stmt'')
  | Return e_opt ->
    Return (bind_opt (fun e -> Some (subst_expr subst e)) None e_opt)
  | Switch (e, cases) ->
    let e' = subst_expr subst e in
    let cases' =
      List.map (fun (e, stmt) ->
        (subst_expr subst e, subst_stmt subst stmt)) cases in
    Switch (e', cases')
  | _ ->
    raise (Inliner_Exc ("Unsupported subst to inline: " ^
              string_of_naasty_statement no_indent stmt, None, Some stmt))

(*Inline variables (that are to be substituted for) within a substitution*)
(*NOTE the order of subst shouldn't matter, because we keep iterating until all
  substitutions are ground (wrt other substitution variables), and this should
  be terminating.*)
let rec inline_substvars_in_subst ?st_opt:(st_opt : state option = None) (subst_acc : substitution) (subst : substitution) : substitution =
  match subst with
  | [] -> subst_acc
  | (id, definiens) :: rest ->
    if List.mem_assoc id subst_acc then
      raise (Inliner_Exc ("Substitution variable appears more than once in the substitution",
                          st_opt, None))
    else
       let is_ground =
         Identifier_Set.exists (fun ident ->
           List.exists (fun (id', _) ->
             (if !Config.cfg.Config.verbosity > 1 then
                print_endline (string_of_int id' ^ " ?= " ^ string_of_int ident);
              id' = ident)) (rest @ subst_acc))
           (free_vars definiens Identifier_Set.empty)
         |> not in
       let id_expr' =
         let definiens' =
           if is_ground then definiens
           else subst_expr (rest @ subst_acc) definiens in
         let _ =
           if !Config.cfg.Config.verbosity > 1 then
             print_endline ("def " ^ string_of_int id ^ " " ^
                            string_of_bool is_ground ^ " : " ^
                            fst (string_of_naasty_expression ~st_opt definiens) ^ " ~~> " ^
                            fst (string_of_naasty_expression ~st_opt definiens')) in
         (id, definiens') in
       let subst' =
         (*Update the substitution*)
         List.fold_right (fun (id', expr') acc ->
           (id', subst_expr (rest @ subst_acc) expr') :: acc) rest
          (if is_ground then [] else [id_expr']) in
       let subst_acc' =
         if is_ground then
           id_expr' :: subst_acc
         else subst_acc
       in inline_substvars_in_subst ~st_opt subst_acc' subst'

let subst_to_string ?st_opt:((st_opt : state option) = None)
      (subst : substitution) =
  let body =
    List.map (fun (id, expr) ->
    "(" ^ string_of_int id ^ ", " ^
    fst (string_of_naasty_expression ~st_opt expr) ^ ")") subst
  in "[" ^ String.concat "; " body ^ "]"

(*Erase declarations and assignments to inlined variables.
  NOTE we need to descend into For, If, etc*)
let rec erase_vars ?aggressive:(aggressive : bool = false) (stmt : naasty_statement) (idents : identifier list) : naasty_statement =
  match stmt with
  | Declaration (ty, _) ->
    begin
    match idx_of_naasty_type ty with
    | None ->
      raise (Inliner_Exc ("Declaration must contain an identifier name, not just mention a type!",
                          None, None))
    | Some idx ->
      if List.mem idx idents then Skip else stmt
    end
  | Seq (stmt1, stmt2) ->
    mk_seq (erase_vars ~aggressive stmt1 idents) (erase_vars ~aggressive stmt2 idents)
  | Assign (Var id, e) ->
    if List.mem id idents then
      if aggressive then Skip
      else
        match e with
        | Var _ -> Skip
        | _ ->
          (*As an overapproximation, retain any expressions containing function
            calls in case they include side-effects*)
          (*FIXME include other side-effecting primitives, such as anything that
                  involves static variables or channels*)
          if contains_functor_app e then St_of_E e
          else Skip
    else stmt

  | Assign (_, _) ->
    (*The lvalue is not a Var (because we've already checked that), so this
      lvalue isn't a temporary variable that we generated. So we don't bother
      trying to erase it.*)
    stmt

  | For ((ty, cond, inc), body) ->
    let body' = erase_vars ~aggressive body idents in
    For ((ty, cond, inc), body')
  | If (cond, stmt1, stmt2) ->
    let stmt1' = erase_vars ~aggressive stmt1 idents in
    let stmt2' = erase_vars ~aggressive stmt2 idents in
    If (cond, stmt1', stmt2')
  | If1 (cond, stmt') ->
    let stmt'' = erase_vars ~aggressive stmt' idents in
    If1 (cond, stmt'')
  | Switch (e, cases) ->
    let cases' =
      List.map (fun (e, stmt) ->
        (e, erase_vars ~aggressive stmt idents)) cases in
    Switch (e, cases')
  | Commented (stmt', comment) ->
    let stmt'' = erase_vars ~aggressive stmt' idents in
    Commented (stmt'', comment)

  | St_of_E _
  | Return _
  | Skip
  | Break
  | Continue
  | GotoLabel _ -> stmt

let table_to_string st table : string =
  List.map (fun entry ->
    inliner_table_entry_to_string ~st_opt:(Some st) entry) table
  |> Debug.print_list "  "(*FIXME const*)

let mk_subst st init_table body =
  let table =
    inliner_analysis st body [] init_table
    |> count_var_references_in_naasty_stmt st body
    |> variables_to_be_inlined
    |> List.sort inliner_table_entry_order in

  let _ =
    if !Config.cfg.Config.verbosity > 0 then
      table_to_string st table
      |> (fun s -> print_endline ("Inliner table:" ^ s))

in
  inliner_to_subst table
  |> (fun subst ->
        subst_to_string ~st_opt:(Some st) subst
        |> (fun s ->
          if !Config.cfg.Config.verbosity > 0 then
            print_endline ("Pre-substitution: " ^ s));
        List.rev subst)
  |> inline_substvars_in_subst ~st_opt:(Some st) []
  |> (fun subst ->
        subst_to_string ~st_opt:(Some st) subst
        |> (fun s ->
          if !Config.cfg.Config.verbosity > 0 then
            print_endline ("Substitution: " ^ s));
        subst)

let mk_erase_ident_list st init_table body : identifier list =
  inliner_analysis st body [] init_table
  |> count_var_references_in_naasty_stmt st body
  |> variables_to_be_erased
  |> List.map (fun record -> record.id)
