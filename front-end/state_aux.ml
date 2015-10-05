(*
   Supporting definitions and functions for the state-related book-keeping
   during the translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open Debug
open State
open Naasty_aux
open Task_model
open Crisp_syntax
open General


let state_to_str ?indentation:(indentation : string = "  ")
      ?summary_types:(summary_types : bool = false) (resolve : bool)
      ({pragma_inclusions; type_declarations; next_symbol;
        type_symbols; term_symbols; crisp_funs} as st: state) =
  let st_opt = if resolve then Some st else None in
  let str_of_ty_opt ty_opt =
    match ty_opt with
    | None -> "?"
    | Some ty -> string_of_naasty_type ~st_opt:st_opt prog_indentation ty in
  let str_of_src_ty_opt src_ty_opt =
    match src_ty_opt with
    | None -> "?"
    | Some ty ->
      Crisp_syntax.type_value_to_string ~summary_types true false Crisp_syntax.min_indentation ty in
  let str_of_term_symbol_metadata md =
    "{source_type=" ^ str_of_src_ty_opt md.source_type ^ "; " ^
    "naasty_type=" ^ str_of_ty_opt md.naasty_type ^ "; " ^
    "identifier_kind=" ^ string_of_identifier_kind ~summary_types md.identifier_kind ^ "}" in
  let type_decls_l =
    List.map (fun (type_name, src_type, nst_type) ->
      let nst_type_s = str_of_ty_opt nst_type in
      let src_type_s = str_of_src_ty_opt (Some src_type) in
      type_name ^ "(" ^ src_type_s ^ ", " ^ nst_type_s ^ ")") type_declarations in
  indentation ^ "pragma_inclusions : " ^ print_list indentation pragma_inclusions ^ "\n" ^
  indentation ^ "type_declarations : " ^ print_list indentation type_decls_l ^ "\n" ^
  indentation ^ "next_symbol : " ^ string_of_int next_symbol ^ "\n" ^
  indentation ^ "type_symbols : " ^ print_list indentation
                         (List.map (fun (s, i, ty_opt) -> "(" ^ s ^ ", " ^
                                   string_of_int i ^ ", " ^ str_of_ty_opt ty_opt ^ ")")
                         type_symbols) ^ "\n" ^
  indentation ^ "term_symbols : " ^ print_list indentation
                     (List.map (fun (s, i, md) -> "(" ^ s ^ ", " ^
                              string_of_int i ^ ", " ^
                              str_of_term_symbol_metadata md ^ ")")
                        term_symbols) ^ "\n" ^
  indentation ^ "crisp_funs : " ^ print_list indentation
                     (List.map (fun (s, (is_fun, ft)) -> "(" ^ s ^ ", " ^
                              string_of_bool is_fun ^ ", " ^
                              Crisp_syntax.function_type_to_string ft ^ ")")
                        crisp_funs) ^ "\n"


(* Infer the type of graph we are building from the list of channels it gets *)
let infer_graph_hint (chan_name : string) : graph_hint =
  PassThroughType  (**FIXME -- this is a temporary hack *)

let get_channel_type (st : state) (chan_name : string) : channel_type =
  match lookup_term_data (Term Channel_Name) st.term_symbols chan_name with
  | None ->
      print_endline ("Channel name " ^ chan_name ^ " not found in symbol table, pretending it is type int for now");
      ChannelSingle (Integer (None, [("hello", Ann_Int(0))]),Empty)  (**FIXME -- this should not be here it should fail*)
      (* print_endline (state_to_str ~summary_types:(!Config.cfg.Config.summary_types) true st); *)
      (* failwith ("Channel name " ^ chan_name ^ " not found in symbol table.")                  *)
  | Some (_, meta_data) -> begin
    match the (meta_data.source_type) with
    | ChanType (_, channel_type) -> channel_type
    | _ -> failwith "Not of type ChanType"
    end

(* input_map takes a string that represents the channel, the state and optionally*)
(* an expression, if and only if the channel is an array -- it returns an expression for the*)
(* offset within the input array and also the type for that channel*)
let input_map (chan_name : string) (st : state) (index : expression option) : expression * Crisp_syntax.type_value =
  let chan_type = ChanType (None, get_channel_type st chan_name) in
  let hint = infer_graph_hint chan_name in
  let expr = match hint with 
    | PassThroughType -> Int 0
    | FoldTreeType ->  begin match chan_name with
      | "x" -> Int 0
      | "y" -> Int 1
      | _ -> failwith ("Expected channel name to be one from x or y but got " ^ chan_name)
      end  
    | DeMuxType -> begin match chan_name with 
      | "client_in" -> Int 0
      | "backend_in" ->  Plus ((Int 1),(the index))
      | _ -> failwith ("Expected channel name to be one from client or backend but got " ^ chan_name)
      end in
   (expr,chan_type)
  
(* output_map is similar to input map for mapping output channels *)
let output_map (chan_name : string) (st : state)  (index : expression option) : expression * Crisp_syntax.type_value =
  let chan_type =  ChanType (None, get_channel_type st chan_name) in
  let hint = infer_graph_hint chan_name in
  let expr = match hint with 
    | PassThroughType -> Int 0
    | FoldTreeType -> Int 0
    | DeMuxType -> begin match chan_name with 
      | "client_out" -> Int 0
      | "backend_out" -> Plus ((Int 1),(the index))
      | _ -> failwith ("Expected channel name to be one from client or backend but got " ^ chan_name)
      end in
  (expr,chan_type)
