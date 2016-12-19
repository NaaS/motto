(*
   Interactive/scriptable runtime for Flick programs.
   Nik Sultana, Cambridge University Computer Lab, July 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open State
open Crisp_syntax
open Runtime_asynch
open Debug

exception Runtime_inspect_exc of string

type chan_idx = int;;

Sys.set_signal Sys.sigint
  (Sys.Signal_handle (fun _ -> Eval_monad.kill_run := true));

type inspect_instruction =
    (*declare and define variable, and initialise*)
  | Declare_value of string * string
    (*set - variable value*)
  | Set of string * string
    (*channel - declare and define channel (in the symbol table and runtime context)*)
  | Declare_channel of string * string
    (*close_channel - break a channel (the connected processes should react to this)*)
  | Close_channel of string
  | Open_channel of string (*FIXME seems redundant*)
    (*queue channel value*)
  | Q_channel of string * Runtime_data.channel_direction * chan_idx option * string
    (*dequeue channel value*)
  | Deq_channel of string * Runtime_data.channel_direction * chan_idx option
    (*load - Flick program from file*)
  | Load of string
    (*Evaluate a Flick expression.
      If a process is called, then it runs for a single iteration.*)
  | Eval of string
  | Clear_Asynch_Eval
  | Asynch_Eval of Eval_monad.work_item_name * string
  | Run_Asynch
    (*form a process out of an expression, and add it to the Asynch_Eval work list.*)
  | Instantiate_Process of Eval_monad.work_item_name * string
    (*execute some meta-instruction, e.g., to show the whole runtime context,
      or specific parts of it, or the symbol_table*)
  | MI of meta_instruction
  (*NOTE could add command to start and pause processes, and remove them
         from the worklist.*)
  (*FIXME add commands to trace running processes and functions. this is quite
          straightforward -- perhaps simply need to add a meta_instruction that
          writes to a log file.*)
  (*FIXME could add command to dynamically load a runtime script*)
  (*FIXME could add a function to call, and a facility to register callbacks,
          to interact with an external system. (for instance, a C-based
          protocol parser that can be linked with the .o file generated from
          a runtime script.*)
  | Declare_dictionary of
      string (*dictionary name*) *
      string (*type of the key*) *
      string (*type of the value*)
  | Declare_reference of
      string (*name of the reference*) *
      (*The type of the reference is inferred from the initial value*)
      string (*value with which to initialise it*)
  | Add_DI of (*FIXME rename to Set_DI?*)
      string (*key*) *
      string (*value (numeric)*)
  | Assign_Resource of
      string (*Flick-level identifier*) *
      Resources.resource
  | Unlink of (*NOTE this was originally 'Unassign_Resource' but I generalised
                     it to work to non-resource identifiers too.*)
      string (*Flick-level identifier*)
  | Acquire_Resource of
      Resources.resource *
      string option (*Initialisation string*)
  | Dismiss_Resource of Resources.resource

type declaration =
  | Binding of expression * type_value
  | Channel of channel_name * channel_type
  | Dictionary of type_value
  | Reference of expression * type_value

let declare (v : string) (st : state) (ctxt : Runtime_data.runtime_ctxt) (d : declaration) : (state * Runtime_data.runtime_ctxt) =
  let ty, ik, value =
     match d with
     | Binding (e, ty) -> ty, Value, Eval.evaluate_value ctxt e
     | Reference (e, ty) -> ty, Value, Eval.evaluate_value ctxt e
     | Channel (cn, cty) ->
         let value =
           match cty with
           | ChannelSingle _ -> Runtime_data.ChannelSingle ([], [])
           | ChannelArray (_, _, di_opt) ->
             match di_opt with
             | None ->
               Runtime_data.ChannelArray []
             | Some di ->
               let size = resolve_di di
               in Runtime_data.ChannelArray (General.repeat size ([], [])) in
         ChanType (Some v, cty), Channel_Name, Runtime_data.ChanType (cn, value)
     | Dictionary ty ->
       ty, Map_Name,
       (*NOTE We initialise to an empty dictionary -- but might want to hook into a
              running service*)
       Dictionary [] in
  (*Remove binding of "v" if it exists*)
  let st' =
    match lookup_term_data (Term ik) st.term_symbols v with
    | None -> st
    | Some (_, md) ->
      begin
      print_endline ("Warning: replacing earlier entry for " ^ v ^ " from symbol table");
      let _ =
        match md.source_type with
        | None ->
          raise (Runtime_inspect_exc ("Found entry in symbol table, but no specific type for " ^ v))
        | Some ty' ->
          if ty = ty' then ()
          else
            let ty_s = type_value_to_string true false min_indentation ty in
            let ty'_s = type_value_to_string true false min_indentation ty' in
            print_endline ("(Moreover type of symbol " ^ v ^ " is being changed from " ^ ty'_s ^ " to " ^ ty_s ^ ")") in
      { st with term_symbols = List.filter (fun (v', _, _) -> v <> v') st.term_symbols }
      end in
  let _, st'' = Naasty_aux.extend_scope_unsafe ~src_ty_opt:(Some ty) (Term ik) st' v in

  (*Update runtime context*)
  let ctxt' =
    if List.mem_assoc v ctxt.Runtime_data.value_table then
      print_endline ("Warning: replacing earlier entry for " ^ v ^ " from runtime state");
    { ctxt with Runtime_data.value_table =
        let pair = (v, value) in
        General.add_unique_assoc pair ctxt.Runtime_data.value_table } in
  (st'', ctxt')

(*Evaluate a single inspect-instruction*)
let eval (st : state) (ctxt : Runtime_data.runtime_ctxt)
   (actxt : asynch_ctxt) (instruction_number : int) (i : inspect_instruction)
   : ((state * Runtime_data.runtime_ctxt) * asynch_ctxt) =
  match i with
  | Declare_value (v, e_s) ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let ty, st' = Type_infer.ty_of_expr st e in
    declare v st' ctxt (Binding (e, ty)), actxt

  | Set (v, e_s) ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let ty, st' = Type_infer.ty_of_expr st e in
    (*Ensure that binding of "v" exists*)
    let _ =
      match lookup_term_data (Term Value) st'.term_symbols v with
      | None ->
        raise (Runtime_inspect_exc ("Could not find entry in symbol table for " ^ v))
      | Some (_, md) ->
        begin
        match md.source_type with
        | None ->
          raise (Runtime_inspect_exc ("Found entry in symbol table, but no specific type for " ^ v))
        | Some ty' -> Type_infer.assert_identical_types ty ty' e st
        end in
    let value = Eval.evaluate_value ctxt e in

    (*Update runtime context*)
    let ctxt' =
      if not (List.mem_assoc v ctxt.Runtime_data.value_table) then
        raise (Runtime_inspect_exc ("Symbol " ^ v ^ " has been declared previously, but not defined"));
      { ctxt with Runtime_data.value_table =
          let pair = (v, value) in
          General.add_unique_assoc pair ctxt.Runtime_data.value_table } in
    (st, ctxt'), actxt

  | Load file_path ->
    let st', (_, fn_decls, proc_decls) =
      Compiler.parse_program file_path
      |> Compiler.front_end ~st:st Config.cfg in
    let exec_table_extension =
      List.map Crisp_project.content_of [fn_decls; proc_decls]
      |> List.concat
      |> List.map (fun d -> (Crisp_syntax_aux.name_of_decl d, d)) in
    let ctxt' =
      { ctxt with Runtime_data.exec_table =
         exec_table_extension @ ctxt.Runtime_data.exec_table } in
    (st', ctxt'), actxt

  | Declare_channel (v, cty_s) ->
    let cty =
      match Crisp_parse.parse_string ("(type| " ^ cty_s ^ "|)") with
      | TypeExpr (ChanType (_, cty)) -> cty
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into a channel type: " ^ cty_s)) in
    declare v st ctxt (Channel (v, cty)), actxt

  | Close_channel v ->
    let ctxt' =
      if List.mem_assoc v ctxt.Runtime_data.value_table then
        { ctxt with Runtime_data.value_table = List.filter (fun (v', _) -> v <> v') ctxt.Runtime_data.value_table }
      else
        raise (Runtime_inspect_exc ("Could not close channel " ^ v ^ " since it's not open")) in
    (st, ctxt'), actxt

  | Open_channel v ->
    let cty =
      match lookup_term_data (Term Channel_Name) st.term_symbols v with
      | None ->
        raise (Runtime_inspect_exc ("No entry in symbol table for " ^ v ^ "; this is needed to work out the channel's type"))
      | Some (_, md) ->
        begin
          match md.source_type with
          | None ->
            raise (Runtime_inspect_exc ("Found entry in symbol table, but no specific type for " ^ v))
          | Some (ChanType (lbl_opt, cty)) ->
            let _ =
              match lbl_opt with
              | None -> ()
              | Some lbl ->
                if lbl <> v then
                  raise (Runtime_inspect_exc ("Type of channel " ^ v ^ " is associated with channel " ^ lbl)) in
            cty
          | Some _ ->
            raise (Runtime_inspect_exc ("Channel " ^ v ^ " has a non-channel type in the symbol table"))
        end in
    let value =
      match cty with
      | ChannelSingle _ -> Runtime_data.ChannelSingle ([], [])
      | ChannelArray _ -> Runtime_data.ChannelArray [] (*FIXME what size array?*) in
    let ctxt' =
      if List.mem_assoc v ctxt.Runtime_data.value_table then
        raise (Runtime_inspect_exc ("Could not open channel " ^ v ^ " since it's already open"))
      else
        { ctxt with Runtime_data.value_table =
                      (v, Runtime_data.ChanType (v, value)) :: ctxt.Runtime_data.value_table } in
    (st, ctxt'), actxt

  | Q_channel (v, dir, idx_opt, e_s) ->
    (*NOTE we don't check that the type of e agrees with the type of the channel it's put in*)
    let e_value, ctxt' =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> Eval.evaluate st ctxt e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let ctxt'', _ =
      let f dir ctxt (incoming, outgoing) =
        match dir with
        | Runtime_data.Incoming ->
          (List.rev (e_value :: List.rev incoming), outgoing, e_value, ctxt)
        | Runtime_data.Outgoing ->
          (incoming, List.rev (e_value :: List.rev outgoing), e_value, ctxt) in
      Runtime_data.channel_fun v dir idx_opt "queue" (fun x -> Runtime_inspect_exc x) f st ctxt' in
    (st, ctxt''), actxt

  | Deq_channel (v, dir, idx_opt) ->
    (*NOTE this operation discards an element from a queue (or raises an exception
           if this is not possible). This element cannot be used elsewhere -- you
           need to use the Flick language (and not the runtime language) for that.*)
    let ctxt', _ =
      let f dir ctxt (incoming, outgoing) =
        match dir with
        | Runtime_data.Incoming ->
          begin
          match incoming with
          | v :: xs -> xs, outgoing, v, ctxt
          | [] ->
            raise (Runtime_inspect_exc ("Could not dequeue from " ^
             Runtime_data.str_of_channel_direction dir ^
             "direction of channel " ^ v ^ " since it is empty"))
          end
        | Runtime_data.Outgoing ->
          (*FIXME DRY principle -- code similar to that used in clause for Incoming*)
          begin
          match outgoing with
          | v :: xs -> incoming, xs, v, ctxt
          | [] ->
            raise (Runtime_inspect_exc ("Could not dequeue from " ^
             Runtime_data.str_of_channel_direction dir ^
             "direction of channel " ^ v ^ " since it is empty"))
          end in
      Runtime_data.channel_fun v dir idx_opt "dequeue" (fun x ->
        Runtime_inspect_exc x) f st ctxt in
    (st, ctxt'), actxt

  | Eval e_s ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let ty, st  = Type_infer.ty_of_expr st e in
    let ty_s =
      type_value_to_string ~summary_types:true ~show_annot:false true false 0 ty in
    let value, ctxt' = Eval.evaluate st ctxt e in
    print_string (encolour Blue (string_of_int instruction_number));
    print_string (encolour Cyan ":");
    foreground_colour Yellow;
    print_endline (e_s ^
      encolour White " ~> " ^
      encolour Yellow (Runtime_data.string_of_typed_value value) ^
      encolour White " typed: " ^
      encolour Red ty_s ^
      reset_colour);
    (st, ctxt'), actxt

  | MI mi ->
    begin
    match mi with
    | Show_symbol_table None
    | Show_symbol_table (Some Runtime_phase) ->
      print_endline (State_aux.state_to_str false st)
    | PrintStr (None, s)
    | PrintStr (Some Runtime_phase, s) -> print_endline s
    | Show_runtime_ctxt None
    | Show_runtime_ctxt (Some Runtime_phase) ->
      print_endline (Runtime_data.string_of_runtime_ctxt ctxt)
    | _ -> () (*ignore other MIs, since they're not relevant to this part of the compiler*)
    end;
    (st, ctxt), actxt

  | Clear_Asynch_Eval ->
    let actxt' = { actxt with work_list = [] } in
    (st, ctxt), actxt'
  | Asynch_Eval (name, e_s) ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let _, st  = Type_infer.ty_of_expr st e in
    let actxt' =
      { actxt with work_list = (name, Eval_monad.evaluate e) :: actxt.work_list } in
    (st, ctxt), actxt'

  | Run_Asynch ->
(*FIXME provide seed for random sequence*)
    (*we ignore the values we get from running the work list*)
    let results, ctxt' = Eval_monad.run_until_done Eval.normalise st ctxt actxt.work_list [] in
    List.fold_right (fun (name, result_e) st ->
      let ty, st  =
        Type_infer.ty_of_expr st result_e in
      let ty_s =
        type_value_to_string ~summary_types:true ~show_annot:false true false 0 ty in
      let value, ctxt' = Eval.evaluate st ctxt result_e in
      (*FIXME colourify, as done in "Eval"*)
      print_endline ("(asynch) " ^ name ^ " ~> " ^
                     expression_to_string min_indentation result_e ^
                     " typed: " ^ ty_s);
      st) results st;
    let actxt' = { actxt with work_list = [] } in
    (st, ctxt'), actxt'

  | Instantiate_Process (name, e_s) ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let actxt' =
      { actxt with work_list =
          (name,
           Eval_monad.Process (name, e)) :: actxt.work_list;
      } in
    (st, ctxt), actxt'

  | Declare_dictionary (name, idx_ty_s, v_ty_s) ->
    let idx_ty =
      match Crisp_parse.parse_string ("(type| " ^ idx_ty_s ^ "|)") with
      | TypeExpr ty -> ty
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into a type: " ^ idx_ty_s)) in
    let v_ty =
      match Crisp_parse.parse_string ("(type| " ^ v_ty_s ^ "|)") with
      | TypeExpr ty -> ty
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into a type: " ^ v_ty_s)) in
    let dict_ty =
      (*FIXME should check if idx_ty is an acceptable index type*)
      Crisp_syntax.Dictionary (Some name, idx_ty, v_ty) in
    declare name st ctxt (Dictionary dict_ty), actxt
  | Declare_reference (name, e_s) ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let ty, st = Type_infer.ty_of_expr st e in
    let ref_ty =
      (*FIXME should check if ty is an acceptable type -- for instance, it can't be a reference type!*)
      Crisp_syntax.Reference (Some name, ty) in
    declare name st ctxt (Reference (e, ref_ty)), actxt

  | Add_DI (k_s, v_s) ->
    (*FIXME Update DI if it already exists in the association list*)
    Config.cfg := { !Config.cfg with
                    dependency_valuation =
                      (k_s, int_of_string v_s) ::
                      !Config.cfg.Config.dependency_valuation};
    (st, ctxt), actxt

  | Assign_Resource (identifier, resource) ->
    (*Update runtime context*)
    let ctxt' =
      if not (List.mem_assoc identifier ctxt.Runtime_data.value_table) then
        raise (Runtime_inspect_exc ("Symbol " ^ identifier ^ " has been declared previously, but not defined"));
      { ctxt with Runtime_data.value_table =
          let pair = (identifier, Runtime_data.Resource resource) in
          General.add_unique_assoc pair ctxt.Runtime_data.value_table } in
    (st, ctxt'), actxt
  | Unlink identifier ->
    (*Remove an identifier from the runtime's state.
      * If this were a resource we could find what kind of resource has been
        assigned, and swap it with an "internal" version of the resource; Any
        data currently held or buffered is not copied into the internal resource
        before the external resource is unassigned. But this is too clever.
      *  If we went down the route of replacing an external resource with an
         internal one, then should probably also take an expression that serves
         to initialise the internal resource, otherwise some suitable default
         initial value should be sought.
      NOTE we don't remove type info from the symbol table at the moment, but
           perhaps this should be done too.*)
    let ctxt' =
      if not (List.mem_assoc identifier ctxt.Runtime_data.value_table) then
        raise (Runtime_inspect_exc ("Symbol " ^ identifier ^ " has been declared previously, but not defined"));
      { ctxt with Runtime_data.value_table =
                    List.filter (fun (v, _) ->
                    v <> identifier) ctxt.Runtime_data.value_table } in
    (st, ctxt'), actxt
  | Acquire_Resource (resource, init_string_opt) ->
    (*Initialise a resource. After being initialised, it should be ready for
      assignment, and thus use.*)
    assert (Resources.acquire_resource resource init_string_opt);
    (st, ctxt), actxt
  | Dismiss_Resource resource ->
    (*Indicate to a resource that it will no longer be used. Whatever resources
      it used can be reclaimed by other parties.*)
    assert (Resources.dismiss_resource resource);
    (st, ctxt), actxt

(*Evaluate a list of inspect-instructions*)
let evals (st : state) (ctxt : Runtime_data.runtime_ctxt)
   (actxt : asynch_ctxt) (is : inspect_instruction list)
   : ((state * Runtime_data.runtime_ctxt) * asynch_ctxt) =
  List.fold_right (fun instr (((st, ctxt), actxt), inst_no) ->
   ((Wrap_err.wrap (eval st ctxt actxt inst_no) instr), inst_no + 1))
   (List.rev is)
   (((st, ctxt), actxt), 1)
  |> fst

let run (is : inspect_instruction list) : unit =
  ignore(evals initial_state Runtime_data.initial_runtime_ctxt
           initial_asynch_ctxt is)
