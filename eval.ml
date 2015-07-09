(*
   Evaluator for Flick programs.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open Debug
open Crisp_syntax

let runtime_ctxt_print_indentation = "  "

(*FIXME name clash with Crisp_syntax.type_value*)
(*Representation of values during evaluation. Evaluation might take place interactively
  with the user (or with the network), so this datatype is used to ensure that only
  values (and not arbitrary expressions) are stored in variables.*)
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

(*We could avoid defining the typed_value type, and working with normalised values
  (that it represents), but this way forces us to make sure we're only working
  with the intended kind of values, and not with arbitrary expressions!*)
(*FIXME runtime_ctxt should include a stack of exception handlers*)
type runtime_ctxt = (symbol_name * typed_value) list
let string_of_runtime_ctxt (ctxt : runtime_ctxt) : string =
  List.map (fun (name, v) ->
    name ^ " = " ^ string_of_typed_value v) ctxt
  |> print_list runtime_ctxt_print_indentation

(*Translate an expression into a value*)
let evaluate (ctxt : runtime_ctxt) (e : expression) : typed_value =
  failwith "TODO"

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

(*Reduce an expression into a value expression*)
let rec normalise (ctxt : runtime_ctxt) (e : expression) : expression =
  failwith "TODO"
