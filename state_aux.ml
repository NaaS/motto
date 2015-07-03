(*
   Supporting definitions and functions for the state-related book-keeping
   during the translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open State
open Naasty_aux


let state_to_str ?summary_types:(summary_types : bool = false) (resolve : bool)
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
  let type_decls_s =
    List.map (fun (type_name, src_type, nst_type) ->
      let nst_type_s = str_of_ty_opt nst_type in
      let src_type_s = str_of_src_ty_opt (Some src_type) in
      type_name ^ "(" ^ src_type_s ^ ", " ^ nst_type_s ^ ")") type_declarations
    |> String.concat "; " in
  "pragma_inclusions : [" ^ String.concat "; " pragma_inclusions ^ "]" ^ "\n" ^
  "type_declarations : [" ^ type_decls_s ^ "]" ^ "\n" ^
  "next_symbol : " ^ string_of_int next_symbol ^ "\n" ^
  "type_symbols : [" ^ String.concat "; "
                         (List.map (fun (s, i, ty_opt) -> "(" ^ s ^ ", " ^
                                   string_of_int i ^ ", " ^ str_of_ty_opt ty_opt ^ ")")
                         type_symbols) ^ "]" ^ "\n" ^
  "term_symbols : [" ^ String.concat "; "
                     (List.map (fun (s, i, md) -> "(" ^ s ^ ", " ^
                              string_of_int i ^ ", " ^
                              str_of_term_symbol_metadata md ^ ")")
                        term_symbols) ^ "]" ^ "\n" ^
  "crisp_funs : [" ^ String.concat "; "
                     (List.map (fun (s, ft) -> "(" ^ s ^ ", " ^
                              Crisp_syntax.function_type_to_string ft ^ ")")
                        crisp_funs) ^ "]" ^ "\n"
