(*
   Supporting definitions and functions for the state-related book-keeping
   during the translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open State
open Naasty_aux


let state_to_str (resolve : bool)
      ({pragmas; type_declarations; next_symbol;
        type_symbols; term_symbols} as st: state) =
  let st_opt = if resolve then Some st else None in
  "pragmas : [" ^ String.concat ", " pragmas ^ "]" ^ "\n" ^
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
