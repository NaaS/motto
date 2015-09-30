(*
   Runtime state for Flick programs.
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
  | ChanType of label * channel_type
(*Channels are abstracted to behave as queues*)
and channel_type =
  | ChannelSingle of typed_value list * typed_value list
  | ChannelArray of ((*typed_value * -- FIXME currently no channel indexing*) (typed_value list * typed_value list)) list

let rec string_of_list_vs vs =
  "[" ^ String.concat ", " (List.map string_of_typed_value vs) ^ "]"
and string_of_chan_vs (in_vs, out_vs) =
  "incoming:" ^ string_of_list_vs in_vs ^ " outgoing:" ^ string_of_list_vs out_vs
and string_of_typed_value : typed_value -> string = function
  | UserDefinedType (_, v) -> string_of_typed_value v
  | String s -> "\"" ^ s ^ "\""
  | Integer i -> string_of_int i
  | Boolean b -> if b then "True" else "False"
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
  | ChanType (cn, cv) -> cn ^ "(" ^ string_of_channel_type cv ^ ")"
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
      NOTE we fix a single, global, scope. Might replace this with nested scopes,
           but this will do fine for now.*)
    value_table : value_table;
    (*Table of function and process declarations*)
    exec_table : (symbol_name * toplevel_decl) list;
    (*Stack of active exceptions*)
    except_table : excepts_decl list list;
    (*Table of function/process-state, both local and shared*)
    state : (symbol_name * value_table) list;
  }

let initial_runtime_ctxt =
  {
    value_table = [];
    exec_table = [];
    except_table = [];
    state = [];
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
      |> List.rev
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

type channel_direction =
  | Incoming
  | Outgoing
let str_of_channel_direction : channel_direction -> string = function
  | Incoming -> "incoming"
  | Outgoing -> "outgoing"

(*Lift operation on a channel's contents to the level of channel_types.*)
let channel_fun v dir idx_opt (operation_verb : string)
      (operation_exn : string -> exn)
      (f : channel_direction -> runtime_ctxt -> (typed_value list * typed_value list) ->
       (typed_value list * typed_value list * typed_value * runtime_ctxt)) st ctxt =
  if List.mem_assoc v ctxt.value_table then
    match idx_opt, lookup_term_data (Term Channel_Name) st.term_symbols v with
    | None, Some (_, {source_type = Some (ChanType (lbl_opt, ChannelSingle (rx_ty, tx_ty)))}) ->
      let _ =
        match lbl_opt with
        | None -> ()
        | Some lbl ->
          if lbl <> v then
            raise (operation_exn ("Channel " ^ v ^ " expected to be called " ^ lbl)) in
      let (vt', (ctxt', result)) = fold_map ([], (ctxt, None))
        (fun (ctxt, result) ((v', value) as unchanged) ->
           if v <> v' then (unchanged, (ctxt, result))
           else
             match value with
             | ChanType (name, ChannelSingle (incoming, outgoing)) ->
               assert (v = name);
               (*since v is (should be) unique in vt, we should only encounter
                 it once, no more, and no less. this assert checks that we don't
                 encounter it more than once.*)
               assert (result = None);
               let (incoming', outgoing', result', ctxt') = f dir ctxt (incoming, outgoing) in
               ((v, ChanType (name, ChannelSingle (incoming', outgoing'))),
                (ctxt', Some result'))
             | _ ->
               (*FIXME give more info*)
               raise (operation_exn "Channel value is of the wrong type"))
        ctxt.value_table in
      { ctxt' with value_table = vt' }, result
    | Some idx, Some (_, {source_type = Some (ChanType (lbl_opt, ChannelArray (rx_ty, tx_ty, _)))}) ->
      let _ =
        match lbl_opt with
        | None -> ()
        | Some lbl ->
          if lbl <> v then
            raise (operation_exn ("Channel " ^ v ^ " expected to be called " ^ lbl)) in
      let (vt', (ctxt', result)) = fold_map ([], (ctxt, None))
        (fun (ctxt, result) ((v', value) as unchanged) ->
           if v <> v' then (unchanged, (ctxt, result))
           else
             match value with
             | ChanType (name, ChannelArray chans) ->
               assert (name = v);
               (*since v is (should be) unique in vt, we should only encounter
                 it once, no more, and no less. this assert checks that we don't
                 encounter it more than once.*)
               assert (result = None);
               if List.length chans <= idx then
                 (*FIXME give more info*)
                 raise (operation_exn "idx out of bounds")
               else
                 let pre, (incoming, outgoing), post = General.list_split_nth_exc idx chans in
                 let (incoming', outgoing', result', ctxt') = f dir ctxt (incoming, outgoing) in
                 ((v, ChanType (name, ChannelArray (pre @ (incoming', outgoing') :: post))),
                  (ctxt', Some result'))
             | _ ->
               (*FIXME give more info*)
               raise (operation_exn "Channel value is of the wrong type"))
        ctxt.value_table in
      { ctxt' with value_table = vt' }, result
    | None, Some (_, {source_type = Some (ChanType (_, ChannelArray (rx_ty, tx_ty, _)))}) ->
      raise (operation_exn ("Could not " ^ operation_verb ^ " onto channel array " ^ v ^ " since an index has not been given"))
    | Some _, Some (_, {source_type = Some (ChanType (_, ChannelSingle (rx_ty, tx_ty)))}) ->
      raise (operation_exn ("Could not " ^ operation_verb ^ " onto channel " ^ v ^ " since an index was given, but wasn't needed"))
    | _, None ->
      raise (operation_exn ("Channel " ^ v ^ " does not exist in the symbol table (but does exist in the runtime context)"))
    | _, _ ->
      raise (operation_exn ("Could not " ^ operation_verb ^ " onto channel " ^ v ^ " since the symbol table appears to contain invalid data"))
  else
    raise (operation_exn ("Could not " ^ operation_verb ^ " onto channel " ^ v ^ " since it appears closed"))

(*Removes the value_table entry for v from a runtime context.
  Checks to ensure that v is defined in the value_table.*)
let undefine_value v ctxt =
   assert (List.mem_assoc v ctxt.value_table);
   { ctxt with value_table =
      List.filter (fun (v', _) -> v <> v')  ctxt.value_table }
