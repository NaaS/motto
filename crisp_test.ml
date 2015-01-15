open Core.Std
open Lexing
open Crisp_syntax

(*from
  https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
*)

(*NOTE this code depends on side-effects to expand tokens, to deal with the
  contraint that the lexer only emits one token at a time.*)
let token_q : Crisp_parser.token Queue.t = Queue.create ()

let expand_macro_tokens (lexer : Lexing.lexbuf -> Crisp_parser.token) (lexbuf : Lexing.lexbuf) : Crisp_parser.token =
  let rec enqueue_token (i : int) (token : Crisp_parser.token) =
    if i = 0 then ()
    else
      begin
        Queue.enqueue token_q token;
        enqueue_token (i - 1) token
      end in
  let expand_macro (times : int) (token : Crisp_parser.token) =
      assert (times > -1); (*we can have UNINDENTN 0 times, in case we just had
                             an \n*)
      if times > 0 then
        begin
          enqueue_token (times - 1) token;
          token
        end
      else
        Crisp_parser.NL (*if we have 0 undents then it still means that we had a
                          newline.*)
  in
  if Queue.is_empty token_q then
    match lexer lexbuf with
    | Crisp_parser.UNDENTN n -> expand_macro n Crisp_parser.UNDENT
    | token -> token
  else
    Queue.dequeue_exn token_q


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf : Crisp_syntax.program =
  (*try Crisp_parser.program Crisp_lexer.main lexbuf with*)
  try Crisp_parser.program (expand_macro_tokens Crisp_lexer.main) lexbuf with
(*
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
*)
  | Crisp_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    (*exit(-1)*)
    []

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
      (*let x = Crisp_lexer.main lexbuf in*)
      let x = expand_macro_tokens Crisp_lexer.main lexbuf in
      if x = Crisp_parser.EOF then (List.rev (x :: acc))
      else contents (x :: acc)
    in contents [] in
  In_channel.close inx;
  results
;;

open Crisp_parser
let string_of_token = function
  | INTEGER x -> "INTEGER(" ^ string_of_int x ^ ")"
  | STRING x -> "STRING(" ^ x ^ ")"
  | BOOLEAN x -> "BOOLEAN(" ^ string_of_bool x ^ ")"

  (*Punctuation*)
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | BANG -> "BANG"
  | QUESTION -> "QUESTION"
  | PERIOD -> "PERIOD"
  | COLONCOLON -> "COLONCOLON"
  | LEFT_R_BRACKET -> "LEFT_R_BRACKET"
  | RIGHT_R_BRACKET -> "RIGHT_R_BRACKET"
  | LEFT_S_BRACKET -> "LEFT_S_BRACKET"
  | RIGHT_S_BRACKET -> "RIGHT_S_BRACKET"
  | LEFT_C_BRACKET -> "LEFT_C_BRACKET"
  | RIGHT_C_BRACKET -> "RIGHT_C_BRACKET"
  | LEFT_A_BRACKET -> "LEFT_A_BRACKET"
  | RIGHT_A_BRACKET -> "RIGHT_A_BRACKET"
  | AT -> "AT"
  | PIPE -> "PIPE"
  | PLUS -> "PLUS"
  | UNDERSCORE -> "UNDERSCORE"
  | DASH -> "DASH"
  | ASTERIX -> "ASTERIX"
  | SLASH -> "SLASH"
  | EOF -> "EOF"
  | COMMA -> "COMMA"
  | NL -> "NL"
  | HASH -> "HASH"

(*  | INDENTN x -> "INDENTN(" ^ string_of_int x ^ ")"*)
  | UNDENTN x -> "UNDENTN(" ^ string_of_int x ^ ")"
  | INDENT -> "INDENT"
  | UNDENT -> "UNDENT"

  (*Reserved words*)
  | IF -> "IF"
  | ELSE -> "ELSE"
  | IN -> "IN"
  | DEF -> "DEF"
  | CARRY_ON -> "CARRY_ON"
  | YIELD -> "YIELD"
  | TYPE -> "TYPE"
  | TYPE_INTEGER -> "TYPE_INTEGER"
  | TYPE_BOOLEAN -> "TYPE_BOOLEAN"
  | TYPE_STRING -> "TYPE_STRING"
  | TYPE_RECORD -> "TYPE_RECORD"
  | TYPE_VARIANT -> "TYPE_VARIANT"
  | CASE -> "CASE"
  | OF -> "OF"
  | AND -> "AND"
  | NOT -> "NOT"
  | OR -> "OR"
  | IMPORT -> "IMPORT"

  (*Names*)
  | UPPER_ALPHA x -> "UPPER_ALPHA(" ^ x ^ ")"
  | LOWER_ALPHA x -> "LOWER_ALPHA(" ^ x ^ ")"
  | NAT_NUM x -> "NAT_NUM(" ^ x ^ ")"
  | VARIABLE x -> "VARIABLE(" ^ x ^ ")"
  | IDENTIFIER x -> "IDENTIFIER(" ^ x ^ ")"

  | _ -> "<UNKNOWN TOKEN>"
;;

let test filepath =
  print_endline ("Testing " ^ filepath);
  printf "%s\n"
    ((List.map ~f:string_of_token (lex_looper filepath ()))
     |> List.fold ~init:"" ~f:(fun s acc -> s ^ ", " ^ acc));
  loop filepath ()
;;

print_endline "*crisp* *crisp*";

let testdir = "tests" in
let dh = Unix.opendir testdir in
try
  while true do
    let file = Unix.readdir dh in
    (*FIXME naive*)
    if file <> "." && file <> ".." then test (testdir ^ "/" ^ file)
    else ()
  done
with End_of_file ->
  Unix.closedir dh;

