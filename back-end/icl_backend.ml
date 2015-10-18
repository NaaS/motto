(*
   Interface to runtime being developed at ICL.
   Nik Sultana, Cambridge University Computer Lab, June 2015
*)

open General
open State
open Naasty
open Task_model
open Crisp_syntax

module ICL_Backend : Backend.Instance =
struct
  type unit_name = string
  type unit_contents = string

(*Every type becomes 2 compilation units in NaaSty: a header file and a code
  file.*)
let translate_type_compilation_unit (st : state)
      (types_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_couple ([], st)
    (fun (st' : state) (decl : Crisp_syntax.toplevel_decl)
      (cunits : Naasty_project.compilation_unit list) ->
       let name = Crisp_syntax_aux.name_of_type decl in
       let (translated, st'') =
         if !Config.cfg.Config.translate then
           Translation.naasty_of_flick_program ~st:st' [decl]
         else ([], st') in
       let module Data_model_instance =
         Data_model.Instance(Data_model_consts.Values(
         struct
           let datatype_name = name
           let ty = Crisp_syntax_aux.the_ty_of_decl decl
         end)) in
       let (type_data_model_instance, st''') =
         fold_map ([], st'') (fun st scheme ->
           Naasty_aux.instantiate_type true scheme.Data_model.identifiers st scheme.Data_model.type_scheme)
           Data_model_instance.instantiate_data_model in
       let header_unit =
         {Naasty_project.name = name;
          Naasty_project.unit_type = Naasty_project.Header;
          (*FIXME currently hardcoded, but this list of inclusions could be
                  extended based on an analysis of the code in the module.*)
          Naasty_project.inclusions =
            ["<stdint.h>";
             "<iostream>";
             "<assert.h>";
             "<exception>";
             "\"TaskBuffer.h\"";
             "\"applications/NaasData.h\""];
          Naasty_project.content =
            [Naasty_aux.add_fields_to_record (the_single translated)
               type_data_model_instance]
         } in
       let (function_data_model_instance, st4) =
         fold_map ([], st''') (fun st scheme ->
           Naasty_aux.instantiate_function true scheme.Data_model.identifiers st
             scheme.Data_model.function_scheme)
           Data_model_instance.instantiate_data_model in
       let cpp_unit =
         {Naasty_project.name = name;
          Naasty_project.unit_type = Naasty_project.Cpp;
          (*FIXME currently hardcoded, but this list of inclusions could be
                  extended based on an analysis of the code in the module.*)
          Naasty_project.inclusions =
            ["\"" ^ Naasty_project.filename_of_compilationunit header_unit ^ "\"";
             "\"LinearBuffer.h\"";
             "<iostream>";
             "\"utils/ReadWriteData.h\"";
             "\"applications/NaasData.h\""];
          Naasty_project.content =
            List.map (fun fn -> Fun_Decl fn) function_data_model_instance
         }
       in (header_unit :: cpp_unit :: cunits, st4))
    types_unit.Crisp_project.content

let translate_function_compilation_unit (st : state)
      (functions_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_map ([], st)
    (fun (st' : state) (flick_f : Crisp_syntax.toplevel_decl) ->
       let name = "function_" ^ Crisp_syntax_aux.name_of_function flick_f in
       let (translated, st'') =
         if !Config.cfg.Config.translate then
           Translation.naasty_of_flick_program ~st:st' [flick_f]
         else ([], st') in
       ({Naasty_project.name = name;
         Naasty_project.unit_type = Naasty_project.Cpp;
         Naasty_project.inclusions = (*FIXME incomplete*)
           ["<stdlib.h>"];
         Naasty_project.content =
           (*FIXME we probably want to include the declarations of functions
                   that are called from this function*)
           translated
        }, (*FIXME generate header file together with this .cpp*)
        st''))
    functions_unit.Crisp_project.content

(*FIXME currently ignoring processes*)
let translate_serialise_stringify
  (st : State.state)
  ({Backend.types_unit; Backend.functions_unit; Backend.processes_unit} :
     Backend.compilation_record) =
  let stringify_compilation_unit (st : state) (cu : Naasty_project.compilation_unit) =
    (Naasty_project.filename_of_compilationunit cu,
     Naasty_project.string_of_compilationunit ~st_opt:(Some st) cu) in
  let (translated_type_units, st') =
    translate_type_compilation_unit st types_unit in
  let (translated_function_units, st'') =
    translate_function_compilation_unit st' functions_unit in
  if !Config.cfg.Config.verbosity > 0 then
    State_aux.state_to_str true st'' |> print_endline;
  List.map (stringify_compilation_unit st'')
    (translated_type_units @ translated_function_units)

let translate = translate_serialise_stringify

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

let preprocess st toplevel_decls =
  fold_map ([], st) preprocess_decl toplevel_decls

end

let translate st (tys, funs, procs) =
  let st' = st in
  ICL_Backend.translate st'
    {Backend.types_unit = tys;
     Backend.functions_unit = funs;
     Backend.processes_unit = procs}
