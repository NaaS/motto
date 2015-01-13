open Core.Std
open Lexing
open Crisp_syntax

(*from
  https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
*)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf : Crisp_syntax.program =
  try Crisp_parser.program Crisp_lexer.main lexbuf with
(*
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
*)
  | Crisp_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  parse_with_error lexbuf
(*
  match parse_with_error lexbuf with
  | [] -> ()
  | value ->
    printf "%a\n" (*Json.output_value*) value;
    parse_and_print lexbuf
*)

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let result = parse_and_print lexbuf in
    In_channel.close inx;
  result
  |> Crisp_syntax.program_to_string
  |> print_endline

(*
let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let result = Crisp_parser.program Crisp_lexer.main lexbuf in
  In_channel.close inx;
  result
  ;;
*)

let lex_looper filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let results =
    let rec contents acc =
      let x = Crisp_lexer.main lexbuf in
      if x = Crisp_parser.EOF then (List.rev (x :: acc))
      else contents (x :: acc)
    in contents [] in
  In_channel.close inx;
  results
;;


let default_file = "test.cp";;

print_endline "*crisp* *crisp*";

loop default_file ()

