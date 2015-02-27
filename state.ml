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

type state =
  { pragmas : string list;
    type_declarations : naasty_type list;
    next_typesymbol : integer;
    type_symbols : (string * integer) list;
    next_symbol : integer;
    symbols : (string * integer) list;
  }

let initial_state =
  { pragmas = [];
    type_declarations = [];
    next_typesymbol = 1;
    type_symbols = [];
    next_symbol = 1;
    symbols = [];
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
let lookup (swapped : bool) (scope : scope) (symbols : ('a * 'b) list)
      (type_symbols : ('a * 'b) list) (id_s : string) (id : 'a) : 'b option =
  let gen_lookup l =
    if not (List.mem_assoc id l) then
      None
    else Some (List.assoc id l) in
  let type_lookup = gen_lookup type_symbols in
  let normal_lookup = gen_lookup symbols in
  if type_lookup <> None && normal_lookup <> None then
    failwith ("Somehow the symbol " ^ id_s ^ " is being used for both a type and a non-type")
  else match scope with
    | Type ->
      if normal_lookup <> None then
        failwith "Type symbol was used in term"
      else type_lookup
    | Term ->
      if type_lookup <> None then
        failwith "Symbol was used in type"
      else normal_lookup

(*Lookup functions for names and indices*)
let lookup_name (scope : scope) (st : state) (id : string) : int option =
  lookup false scope st.symbols st.type_symbols id id
let lookup_id (scope : scope) (st : state) (id : int) : string option =
  lookup true scope (List.map swap st.symbols)
    (List.map swap st.type_symbols) (string_of_int id) id

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
