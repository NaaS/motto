(*
   Supporting definitions and functions for the translation from Flick to the
   NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty


(*If forbid_shadowing then names can only be declared once. This could be overly
  restrictive for normal use. If not forbid_shadowing, then the intermediate
  language's names become handled in a hashcons-like way.*)
let forbid_shadowing = false

(*Should types and terms be treated differently -- can types be treated as
  terms? If no, then the "sizeof" operator becomes very awkward.*)
let term_type_separation = false

type state =
  { pragma_inclusions : string list;
    type_declarations : (type_name * type_value * naasty_type) list;
    next_symbol : identifier;
    type_symbols : (string * identifier * naasty_type option) list;
    term_symbols : (string * identifier * naasty_type option) list;
    crisp_funs : (function_name * function_type) list;
  }

let initial_state =
  { pragma_inclusions = [];
    type_declarations = [];
    next_symbol = 1;
    type_symbols = [];
    term_symbols = [];
    crisp_funs = [];
  }

type scope =
  | Type
  | Term
let scope_to_str scope =
  match scope with
  | Type -> "type"
  | Term -> "symbol"
(*For simplicity (and to defend against the possibility that identifiers and
  type identifiers occupy the same namespace) the lookup is made on both
  namespaces.*)
let lookup (swapped : bool) (scope : scope) (symbols : ('a * 'b * 'c) list)
      (type_symbols : ('a * 'b * 'c) list)
      (id_to_str : 'a -> string) (res_to_str : 'b -> string)
      (id : 'a) : 'b option =
  let gen_lookup l =
    let l' = List.map (fun (x, y, _) -> (x, y)) l in
    if not (List.mem_assoc id l') then
      None
    else Some (List.assoc id l') in
  let type_lookup = gen_lookup type_symbols in
  let normal_lookup = gen_lookup symbols in
  if term_type_separation && type_lookup <> None && normal_lookup <> None then
    failwith ("Somehow the symbol " ^ id_to_str id ^
              " is being used for both a type and a non-type")
  else match scope with
    | Type ->
      begin
        match normal_lookup with
        | None -> type_lookup
        | Some idx ->
          if not term_type_separation then
            type_lookup
          else
            failwith ("Type symbol " ^ id_to_str id ^
                      " was used in term, getting idx " ^
                      res_to_str idx)
      end
    | Term ->
      begin
        match type_lookup with
        | None -> normal_lookup
        | Some idx ->
          if not term_type_separation then
            normal_lookup
          else
            failwith ("Symbol " ^ id_to_str id ^
                      " was used in type, getting idx " ^
                      res_to_str idx)
      end

(*Lookup functions for names and indices. Note that (string) names are used for
identifiers in the Crisp AST, but (numeric) indices are used in the NaaSty AST.*)
let lookup_name (scope : scope) (st : state) (id : string) : identifier option =
  lookup false scope st.term_symbols st.type_symbols (fun x -> x) string_of_int id
let lookup_id (scope : scope) (st : state) (id : identifier) : string option =
  lookup true scope (List.map swap_1_2 st.term_symbols)
    (List.map swap_1_2 st.type_symbols) string_of_int (fun x -> x) id

(*Updates the type associated with a symbol in the symbol table*)
let update_symbol_type (id : identifier) (ty : naasty_type)
      (scope : scope) (st : state) : state =
  let generic_transf =
    List.map (fun (name, idx, ty_opt) ->
      if id = idx then
        match ty_opt with
        | None -> (name, idx, Some ty)
        | Some ty' ->
            if ty <> ty' then
              failwith "Different types associated with same symbol!"
            (*Here we are being lenient, because we could fail even if we try to
              assign the same type to the same symbol twice.*)
            else (name, idx, ty_opt)
      else (name, idx, ty_opt)) in
  let type_symbols_transf =
    match scope with
    | Term -> fun x -> x
    | Type -> generic_transf in
  let term_symbols_transf =
    match scope with
    | Type -> fun x -> x
    | Term -> generic_transf in
  { st with
    type_symbols = type_symbols_transf st.type_symbols;
    term_symbols = term_symbols_transf st.term_symbols;
  }

let lookup_symbol_type (id : identifier)
      (scope : scope) (st : state) : naasty_type option =
  let generic_lookup =
    List.fold_right (fun (_, idx, ty_opt) acc ->
      if id = idx then ty_opt
      else acc) in
  match scope with
  | Term -> generic_lookup st.term_symbols None
  | Type -> generic_lookup st.type_symbols None
