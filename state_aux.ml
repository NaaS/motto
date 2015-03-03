(*
   Supporting definitions and functions for the state-related book-keeping
   during the translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open State
open Naasty_aux


let state_to_str (resolve : bool)
      ({pragma_inclusions; type_declarations; next_symbol;
        type_symbols; term_symbols} as st: state) =
  let st_opt = if resolve then Some st else None in
  "pragma_inclusions : [" ^ String.concat ", " pragma_inclusions ^ "]" ^ "\n" ^
  "type_declarations : [" ^
  String.concat ", " (List.map (string_of_naasty_type ~st_opt:st_opt
                                  prog_indentation) type_declarations) ^ "]" ^ "\n" ^
  "next_symbol : " ^ string_of_int next_symbol ^ "\n" ^
  "type_symbols : [" ^ String.concat ", "
                         (List.map (fun (s, i) -> "(" ^ s ^ ", " ^ string_of_int
                                  i ^ ")")
                         type_symbols) ^ "]" ^ "\n" ^
  "term_symbols : [" ^ String.concat ", "
                     (List.map (fun (s, i) -> "(" ^ s ^ ", " ^ string_of_int
                              i ^ ")")
                     term_symbols) ^ "]" ^ "\n"

(*Extends a scope by adding a mapping between a name and an index.
  NOTE we don't check for clashes! thus the _unsafe prefix*)
let extend_scope_unsafe (scope : scope) (st : state) (id : string) : Naasty.identifier * state =
  match scope with
  | Type ->
    (st.next_symbol,
     { st with
       type_symbols = (id, st.next_symbol) :: st.type_symbols;
       next_symbol = 1 + st.next_symbol;
     })
  | Term ->
    (st.next_symbol,
     { st with
       term_symbols = (id, st.next_symbol) :: st.term_symbols;
       next_symbol = 1 + st.next_symbol;
     })

(*Adds a fresh identifier to the scope, based on a specific prefix, to which
  we concatenate a numeric suffix/index*)
let mk_fresh (scope : scope) (id : string) (min_idx : int) (st : state) :
  string * Naasty.identifier * state =
  if min_idx < 0 then
    failwith "min_idx must be non-negative"
  else
    let idx = ref min_idx in
    while (lookup_name scope st (id ^ string_of_int !idx) <> None) do
      idx := 1 + !idx
    done;
    let name = id ^ string_of_int !idx in
    let (idx, st') = extend_scope_unsafe scope st name
    in (name, idx, st')
