(*
  Test framework for the Crisp parser
  Nik Sultana, Cambridge University Computer Lab, January 2015

  Parts of this module is based on explanation & code given at
  https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
*)

open Core.Std
open Lexing
open Crisp_syntax

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
  let expand_macro (times : int) (token : Crisp_parser.token)
        (trailing_token_opt : Crisp_parser.token option) =
      assert (times > -1); (*we can have UNINDENTN 0 times, in case we just had
                             an \n*)
      let insert_trailing_token () =
          match trailing_token_opt with
            | None -> ()
            | Some tok -> Queue.enqueue token_q tok in
      if times > 0 then
        begin
          enqueue_token (times - 1) token;
          insert_trailing_token ();
          token
        end
      else
        Crisp_parser.NL (*if we have 0 undents then it still means that we had a
                          newline.*)
  in
  if Queue.is_empty token_q then
    match lexer lexbuf with
    | Crisp_parser.UNDENTN n ->
        expand_macro n Crisp_parser.UNDENT
         (Some Crisp_parser.NL) (*always have an NL following
                                  (one or more) UNDENTs. This allows us to
                                  parse nested records, for example, since
                                  the contents of the containing record
                                  must be separated by an NL.*)
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
  print_endline "Starting program";
  result
  |> Crisp_syntax.program_to_string
  |> print_endline;
  print_endline "Finished program"

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
      if x = Crisp_parser.EOF then List.rev (x :: acc)
      else contents (x :: acc)
    in contents [] in
  In_channel.close inx;
  results
;;

open Crisp_parser
let string_of_token = function
(*
  | INTEGER x -> "INTEGER(" ^ string_of_int x ^ ")"
  | STRING x -> "STRING(" ^ x ^ ")"
  | BOOLEAN x -> "BOOLEAN(" ^ string_of_bool x ^ ")"
*)

  (*Punctuation*)
  | COLON -> "COLON"
  | LEFT_R_BRACKET -> "LEFT_R_BRACKET"
  | RIGHT_R_BRACKET -> "RIGHT_R_BRACKET"
  | LEFT_S_BRACKET -> "LEFT_S_BRACKET"
  | RIGHT_S_BRACKET -> "RIGHT_S_BRACKET"
  | LEFT_C_BRACKET -> "LEFT_C_BRACKET"
  | RIGHT_C_BRACKET -> "RIGHT_C_BRACKET"
  | DASH -> "DASH"
  | EOF -> "EOF"
  | COMMA -> "COMMA"
  | NL -> "NL"

  | UNDENTN x -> "UNDENTN(" ^ string_of_int x ^ ")"
  | INDENT -> "INDENT"
  | UNDENT -> "UNDENT"

  (*Reserved words*)
  | TYPE -> "TYPE"
  | TYPE_INTEGER -> "TYPE_INTEGER"
  | TYPE_BOOLEAN -> "TYPE_BOOLEAN"
  | TYPE_STRING -> "TYPE_STRING"
  | TYPE_RECORD -> "TYPE_RECORD"
  | TYPE_VARIANT -> "TYPE_VARIANT"
  | TYPE_UNIT -> "TYPE_UNIT"
  | TYPE_LIST -> "TYPE_LIST"
(*
  (*Names*)
  | UPPER_ALPHA x -> "UPPER_ALPHA(" ^ x ^ ")"
  | LOWER_ALPHA x -> "LOWER_ALPHA(" ^ x ^ ")"
  | NAT_NUM x -> "NAT_NUM(" ^ x ^ ")"
  | VARIABLE x -> "VARIABLE(" ^ x ^ ")"
*)
  | IDENTIFIER x -> "IDENTIFIER(" ^ x ^ ")"

  | PROC -> "PROC"
  | SLASH -> "SLASH"
  | ARR_RIGHT -> "ARR_RIGHT"
  | UNITY -> "UNITY"

  | TYPE_IPv4ADDRESS -> "TYPE_IPv4ADDRESS"
  | TRUE -> "TRUE"
  | PLUS -> "PLUS"
  | PERIOD -> "PERIOD"
  | OR -> "OR"
  | NOT -> "NOT"
  | LT -> "LT"
  | LOCAL -> "LOCAL"
  | LET -> "LET"
  | IPv4 _ -> "IPv4 _"
  | INTEGER _ -> "INTEGER _"
  | IN -> "IN"
  | IF -> "IF"
  | GT -> "GT"
  | GLOBAL -> "GLOBAL"
  | FUN -> "FUN"
  | FALSE -> "FALSE"
  | EQUALS -> "EQUALS"
  | ELSE -> "ELSE"
  | ASSIGN -> "ASSIGN"
  | AR_RIGHT -> "AR_RIGHT"
  | AND -> "AND"

  | BANG -> "BANG"
  | SEMICOLON -> "SEMICOLON"
  | QUESQUES -> "QUESQUES"
  | QUES -> "QUES"
  | EXCEPT -> "EXCEPT"

  | ASTERISK -> "ASTERISK"
  | MOD -> "MOD"
  | ABS -> "ABS"

  | IPv4 _ -> "IPv4 _"
  | ADDRESS_TO_INT -> "ADDRESS_TO_INT"
  | INT_TO_ADDRESS -> "INT_TO_ADDRESS"

  | COLONCOLON -> "COLONCOLON"
  | LEFT_RIGHT_S_BRACKETS -> "LEFT_RIGHT_S_BRACKETS"
  | AT -> "AT"
  | TYPE_TUPLE -> "TYPE_TUPLE"
  | WITH -> "WITH"
  | SWITCH -> "SWITCH"

  | PERIODPERIOD -> "PERIODPERIOD"
  | FOR -> "FOR"
  | INITIALLY -> "INITIALLY"
  | MAP -> "MAP"
;;

let test filepath =
  print_endline ("Testing " ^ filepath);
  printf "%s\n"
    ((List.map ~f:string_of_token (lex_looper filepath ()))
     |> List.fold ~init:"" ~f:(fun s acc -> s ^ ", " ^ acc));
  loop filepath ()
;;

print_endline "*crisp* *crisp*";

if Array.length Sys.argv = 1 then
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
else
  for i = 1 to Array.length Sys.argv - 1 do
    test Sys.argv.(i)
  done
