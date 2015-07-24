(*
   Interactive/scriptable runtime for Flick programs.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open State
open Crisp_syntax

exception Runtime_inspect_exc of string

type chan_idx = int

type chan_arg = Runtime_data.channel_direction * string

type inspect_instruction =
    (*declare and define variable, and initialise*)
  | Declare_value of string * string
    (*set - variable value*)
  | Set of string * string
    (*channel - declare and define channel (in the symbol table and runtime context)*)
  | Declare_channel of string * string
    (*close_channel - break a channel (the connected processes should react to this)*)
  | Close_channel of string
  | Open_channel of string
    (*queue channel value*)
  | Q_channel of string * Runtime_data.channel_direction * chan_idx option * string
    (*dequeue channel value*)
  | Deq_channel of string * Runtime_data.channel_direction * chan_idx option
    (*load - Flick program from file*)
  | Load of string
    (*Evaluate a Flick expression.
      If a process is called, then it runs for a single iteration.*)
  | Eval of string
  | Asynch_Eval of string
  | Run_Asynch
    (*enstantiate a process (connecting it to specific channels) and add it to
      the Asynch_Eval work list.*)
  | Instantiate_Process of string * chan_arg list * string list
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

type declaration =
  | Binding of expression * type_value
  | Channel of channel_type

let declare (v : string) (st : state) (ctxt : Runtime_data.runtime_ctxt) (d : declaration) : (state * Runtime_data.runtime_ctxt) =
  let ty, ik, value =
     match d with
     | Binding (e, ty) -> ty, Value, Eval.evaluate_value ctxt e
     | Channel cty ->
         let value =
           match cty with
           | ChannelSingle _ -> Runtime_data.ChannelSingle ([], [])
           | ChannelArray _ -> Runtime_data.ChannelArray [] (*FIXME what size array?*) in
         ChanType cty, Channel_Name, Runtime_data.ChanType value in
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
   (work_list : Eval_monad.eval_monad list) (i : inspect_instruction)
   : ((state * Runtime_data.runtime_ctxt) * Eval_monad.eval_monad list) =
  match i with
  | Declare_value (v, e_s) ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let ty, st' = Type_infer.ty_of_expr st e in
    declare v st' ctxt (Binding (e, ty)), work_list

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
        | Some ty' ->
          if ty = ty' then ()
          else
            let ty_s = type_value_to_string true false min_indentation ty in
            let ty'_s = type_value_to_string true false min_indentation ty' in
            raise (Runtime_inspect_exc ("Entry in symbol table for " ^ v ^ " is typed " ^ ty'_s ^ " but type of " ^ e_s ^ " is " ^ ty_s))
        end in
    let value = Eval.evaluate_value ctxt e in

    (*Update runtime context*)
    let ctxt' =
      if not (List.mem_assoc v ctxt.Runtime_data.value_table) then
        raise (Runtime_inspect_exc ("Symbol " ^ v ^ " has been declared previously, but not defined"));
      { ctxt with Runtime_data.value_table =
          let pair = (v, value) in
          General.add_unique_assoc pair ctxt.Runtime_data.value_table } in
    (st, ctxt'), work_list

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
    (st', ctxt'), work_list

  | Declare_channel (v, cty_s) ->
    let cty =
      match Crisp_parse.parse_string ("(type| " ^ cty_s ^ "|)") with
      | TypeExpr (ChanType cty) -> cty
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into a channel type: " ^ cty_s)) in
    declare v st ctxt (Channel cty), work_list

  | Close_channel v ->
    let ctxt' =
      if List.mem_assoc v ctxt.Runtime_data.value_table then
        { ctxt with Runtime_data.value_table = List.filter (fun (v', _) -> v <> v') ctxt.Runtime_data.value_table }
      else
        raise (Runtime_inspect_exc ("Could not close channel " ^ v ^ " since it's not open")) in
    (st, ctxt'), work_list

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
          | Some (ChanType cty) -> cty
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
        { ctxt with Runtime_data.value_table = (v, Runtime_data.ChanType value) :: ctxt.Runtime_data.value_table } in
    (st, ctxt'), work_list

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
    (st, ctxt''), work_list

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
    (st, ctxt'), work_list

  | Eval e_s ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    let value, ctxt' = Eval.evaluate st ctxt e in
    print_endline (e_s ^ " ~> " ^ Runtime_data.string_of_typed_value value);
    (st, ctxt'), work_list

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
    (st, ctxt), work_list

  | Asynch_Eval e_s ->
    let e =
      match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
      | Expression e -> e
      | _ ->
        raise (Runtime_inspect_exc ("Could not parse into an expression: " ^ e_s)) in
    (st, ctxt), (Eval_monad.evaluate e :: work_list)

  | Run_Asynch ->
    (*we ignore the values we get from running the work list*)
    let _, ctxt' = Eval_monad.run_until_done Eval.normalise st ctxt work_list [] in
    (st, ctxt'), []

(*Evaluate a list of inspect-instructions*)
let evals (st : state) (ctxt : Runtime_data.runtime_ctxt)
   (work_list : Eval_monad.eval_monad list) (is : inspect_instruction list)
   : ((state * Runtime_data.runtime_ctxt) * Eval_monad.eval_monad list) =
  List.fold_right (fun instr ((st, ctxt), wl) ->
   Wrap_err.wrap (eval st ctxt wl) instr) (List.rev is) ((st, ctxt), work_list)

let run (is : inspect_instruction list) : unit =
  ignore(evals initial_state Runtime_data.initial_runtime_ctxt [] is)
