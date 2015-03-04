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

(*This is a hack around the protection we have for separate namespaces for types
  and term-level identifiers. The reason for this hack is that I'd need to
  extend the template language to specify whether the 0 (i.e., fresh name)
  placeholder should generate a fresh name in Term or Type scope. For the time
  being, setting this flag will simplify the template language at the cost of
  some protection!*)
let merge_type_term_scopes = true

type state =
  { pragma_inclusions : string list;
    type_declarations : naasty_type list;
    next_symbol : integer;
    (*NOTE that we don't track the definitions of types in the mapping below;
           that wouldn't be a bad idea.*)
    type_symbols : (string * integer) list;
    (*NOTE we don't track the types in the mapping below; that wouldn't be a bad
           idea. Essentially it would turn this into a symbol table.*)
    term_symbols : (string * integer) list;
  }

let initial_state =
  { pragma_inclusions = [];
    type_declarations = [];
    next_symbol = 1;
    type_symbols = [];
    term_symbols = [];
  }

type scope =
  | Type
  | Term
  | Both
let scope_to_str scope =
  match scope with
  | Type -> "type"
  | Term -> "symbol"
  | Both -> "both"
(*For simplicity (and to defend against the possibility that identifiers and
  type identifiers occupy the same namespace) the lookup is made on both
  namespaces.*)
let lookup (swapped : bool) (scope : scope) (symbols : ('a * 'b) list)
      (type_symbols : ('a * 'b) list)
      (id_to_str : 'a -> string) (res_to_str : 'b -> string)
      (id : 'a) : 'b option =
  let gen_lookup l =
    if not (List.mem_assoc id l) then
      None
    else Some (List.assoc id l) in
  let type_lookup = gen_lookup type_symbols in
  let normal_lookup = gen_lookup symbols in
  match type_lookup, normal_lookup with
  | Some ty_idx, Some te_idx ->
    if not merge_type_term_scopes then
      failwith ("Somehow the symbol " ^ id_to_str id ^
                " is being used for both a type and a non-type.")
    else if ty_idx <> te_idx then
      (*Even if we merge the scopes, we expect the index used in both scopes
        to be the same!*)
      failwith ("Scope corruption? The symbol " ^ id_to_str id ^
                " is being used for both a type and a non-type that use different indices.")
    else
      begin
        assert (type_lookup = normal_lookup);
        type_lookup
      end
  | _, _ ->(*FIXME non-idiomatic coding style*)
    match scope with
    | Type ->
      begin
        match normal_lookup with
        | None -> type_lookup
        | Some idx ->
          failwith ("Type symbol " ^ id_to_str id ^
                    " was used in term, getting idx " ^
                    res_to_str idx)
      end
    | Term ->
      begin
        match type_lookup with
        | None -> normal_lookup
        | Some idx ->
          failwith ("Symbol " ^ id_to_str id ^
                    " was used in type, getting idx " ^
                    res_to_str idx)
      end
    | Both -> None (*Since the lookup fails in either the Type or Term scope.*)

(*Lookup functions for names and indices. Note that (string) names are used for
identifiers in the Crisp AST, but (numeric) indices are used in the NaaSty AST.*)
let lookup_name (scope : scope) (st : state) (id : string) : identifier option =
  lookup false scope st.term_symbols st.type_symbols (fun x -> x) string_of_int id
let lookup_id (scope : scope) (st : state) (id : identifier) : string option =
  lookup true scope (List.map swap st.term_symbols)
    (List.map swap st.type_symbols) string_of_int (fun x -> x) id

(*Given a name, it returns the same name if it is fresh (wrt types and symbols)
  otherwise it modifies it to be fresh.*)
let mk_fresh_name (st : state) (id : string) : string =
  let i = ref "_" in
  let normal_lookup = ref (lookup_name Term st id) in
  let type_lookup = ref (lookup_name Type st id) in
  while !normal_lookup <> None || !type_lookup <> None do
    normal_lookup := lookup_name Term st (id ^ !i);
    type_lookup := lookup_name Type st (id ^ !i);
    i := !i ^ "_";
  done;
  id ^ !i

(*Ensures that a name is fresh wrt the state.*)
let ensure_fresh_name (st : state) (id : string) : string =
  let normal_lookup = lookup_name Term st id in
  let type_lookup = lookup_name Type st id in
  if normal_lookup <> None || type_lookup <> None then
    failwith ("Name '" ^ id ^ "' is not fresh")
  else id
