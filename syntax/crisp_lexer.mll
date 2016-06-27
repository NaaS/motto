{
(*
   Parser spec for Crisp
   Nik Sultana, Cambridge University Computer Lab, January 2015

   Target parser-generator: menhir 20140422

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open Lexing
open Crisp_parser

exception Error

let backtrack str lexbuf =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - (String.length str)

let count_nl str =
  if str <> (String.make (String.length str) '\n') then
    failwith "Counting new lines in a string containing not only new lines"
  else
    String.length str (*NOTE assumes we only use unix-style newlines*)

let next_line ?(nl_count=1) ?nl_str ?(nl_at_end=false) lexbuf =
  (*this function was adapted from
    https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html*)
  let nl_count =
    match nl_str with
    | None -> nl_count
    | Some str -> count_nl str in
  if nl_count > 0 then
    let pos = lexbuf.lex_curr_p in
    let bol = if nl_at_end then lexbuf.lex_curr_pos
                           else lexbuf.lex_start_pos + nl_count in
    let new_line_num = pos.pos_lnum + nl_count in 
    if !Config.cfg.Config.verbosity > 0 then
      Printf.printf ">>DEBUG>> next_line %d @ %d (line %d, bol %d/%d, sp %d)\n"
        nl_count lexbuf.lex_curr_pos pos.pos_lnum
        lexbuf.lex_curr_p.pos_bol bol lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <-
      {
        (* The position of the first token on the line, even if NL *)
        pos with pos_bol = bol; 
        pos_lnum = new_line_num;
      }
;;
let scope_stack : int Stack.t =
  Stack.create ()
;;
Stack.push Crisp_syntax.min_indentation scope_stack;;

let test_indentation indentation follow_on_tokens lexbuf =
  assert (not (Stack.is_empty scope_stack)); (*There should always be at least
                                               one element in the stack: 0*)
  (*Count how many scopes we've moved down (out of).*)
  let rec undented_scopes (offset : int) =
    if Stack.top scope_stack = indentation then
      offset
    else if Stack.top scope_stack < indentation then
      raise Error
    else
      begin
        ignore(Stack.pop scope_stack);
        undented_scopes (offset + 1)
      end in
  let prev = Stack.top scope_stack in
  if indentation > prev then
    begin
      Stack.push indentation scope_stack;
      INDENT
    end
  else if indentation = prev then
    begin
      if follow_on_tokens <> [] then
        begin
          assert (List.length follow_on_tokens = 1);
          List.hd follow_on_tokens
        end
      else NL
    end
  else
    begin
      assert (indentation < prev);
      UNDENTN (undented_scopes Crisp_syntax.min_indentation,
               follow_on_tokens)
    end
}

(*NOTE tabs are not recognised, because they suck. They also make it more
  difficult to measure indentation when mixed with spaces..*)
let ws = ' '+
let comment = '#'[^'\n']*
let integer = ['0'-'9']+

(*NOTE currently only Unix-style newline is supported, because it's simpler.*)
let nl = '\n'
(*NOTE we match nl with eof to maintain a correct line number in unix text files.
  The whitespace and comment before it are to ensure it is the longest match. *)
let end_of_file = (nl|' '|comment)* eof

rule start_of_line = parse
  | ' '* comment nl {next_line ~nl_at_end:true lexbuf; start_of_line lexbuf}
  | ' '* (nl+ as newlines)
    {next_line ~nl_at_end:true ~nl_str:newlines lexbuf;
     start_of_line lexbuf}
  | ws "type" {raise Error} (*throw error earlier to put error on correct line*)
  | (ws as spaces) {test_indentation (String.length spaces) [] lexbuf}
  | (nl+ as newlines) {next_line ~nl_str:newlines lexbuf; NL}
  | "type" {test_indentation Crisp_syntax.min_indentation [TYPE] lexbuf}
  | "process" {test_indentation Crisp_syntax.min_indentation [PROC] lexbuf}
  | end_of_file {test_indentation Crisp_syntax.min_indentation [EOF] lexbuf}
(*NOTE since functions are effectful, could replace proc with fun, and signal
  the entry point via some name -- e.g., main -- but then arbitrary functions
  would be able to register listeners for events.*)
  | "fun" {test_indentation Crisp_syntax.min_indentation [FUN] lexbuf}
  | "(|" {FAT_BRACKET_OPEN}
  | "(type|" {FAT_TYPE_BRACKET_OPEN}
  | "|)" {FAT_BRACKET_CLOSE}
  | "include" {INCLUDE}

and main = parse
  | comment nl {next_line ~nl_at_end:true lexbuf; start_of_line lexbuf}
  | (nl+ as newlines) (' '* as spaces) comment nl
    {next_line ~nl_str:newlines lexbuf; next_line ~nl_at_end:true lexbuf;
     start_of_line lexbuf}
  | (nl+ as newlines) (ws as spaces)
    {next_line ~nl_str:newlines lexbuf; test_indentation (String.length spaces) [] lexbuf}
  | "type" {TYPE}
  | "typed" {TYPED}
  | "integer" {TYPE_INTEGER}
  | "string" {TYPE_STRING}
  | "boolean" {TYPE_BOOLEAN}
  | "record" {TYPE_RECORD}
  | "variant" {TYPE_VARIANT}
  | "list" {TYPE_LIST}
  | "tuple" {TYPE_TUPLE}
  | ":" {COLON}
  | "/" {SLASH}
  | "[" {LEFT_S_BRACKET}
  | "]" {RIGHT_S_BRACKET}
  | "[]" {LEFT_RIGHT_S_BRACKETS}
  | "::" {COLONCOLON}
  | "@" {AT}
  | "(" {LEFT_R_BRACKET}
  | ")" {RIGHT_R_BRACKET}
  | "{" {LEFT_C_BRACKET}
  | "}" {RIGHT_C_BRACKET}
  | "," {COMMA}
  | "=>" {ARR_RIGHT}
  | "->" {AR_RIGHT}
  | "-" {DASH}
  | nl {next_line lexbuf; NL}
  | ws {main lexbuf}
  | end_of_file {test_indentation Crisp_syntax.min_indentation [EOF] lexbuf}
  | "ipv4_address" {TYPE_IPv4ADDRESS}
  | (integer as oct1) '.' (integer as oct2) '.' (integer as oct3) '.' (integer as oct4)
      {IPv4 (int_of_string(oct1), int_of_string(oct2), int_of_string(oct3),
             int_of_string(oct4))}
  | integer as num {INTEGER (int_of_string(num))}
  | "." {PERIOD}
  | "if" {IF}
  | "else" {ELSE}
  | "local" {LOCAL}
  | "global" {GLOBAL}
  | ":=" {ASSIGN}
  | ";" {SEMICOLON}
  | "=" {EQUALS}
  | "let" {LET}
  | "in" {IN}
  | "except" {EXCEPT}

  | ".." {PERIODPERIOD}
  | "for" {FOR}
  | "initially" {INITIALLY}
  | "map" {MAP}
  | "unordered" {UNORDERED}

  | "+" {PLUS}
  | "*" {ASTERISK}
  | "mod" {MOD}
  | "abs" {ABS}

  | ">" {GT}
  | "<" {LT}

  | "and" {AND}
  | "or" {OR}
  | "not" {NOT}
  | "True" {TRUE}
  | "False" {FALSE}

  | "address_to_int" {ADDRESS_TO_INT}
  | "int_to_address" {INT_TO_ADDRESS}

  | "with" {WITH}

  | "switch" {SWITCH}

  | "<-" {ARG_NAMING}

  | "<=" {ARR_LEFT}
  | "<=>" {ARR_BOTH}

  | "dictionary" {TYPE_DICTIONARY}
  | "ref" {TYPE_REF}

  | "can" {CAN}
  | "unsafe_cast" {UNSAFE_CAST}

  | "|)" {FAT_BRACKET_CLOSE}

  | "@:" {META_OPEN}
  | ":@" {META_CLOSE}

  | "size" {SIZE}

  (*FIXME need to check for escapes, particularly that of doublequotes*)
  | '"' ([^'"']* as str) '"' {STRING str}

  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_''\'']* as id {IDENTIFIER id}

  | "_" {UNDERSCORE}

  | "!" {BANG}
  | "?" {QUESTION}
  | "??" {QUESTIONQUESTION}

  | "|" {BAR}

(*FIXME string primitives as keywords -- e.g., concat, etc*)

{
let at_start_of_line lexbuf =
  lexbuf.lex_curr_pos = 0
  || (lexbuf.lex_curr_pos = lexbuf.lex_curr_p.pos_bol)
let main lexbuf =
  if at_start_of_line lexbuf then start_of_line lexbuf
                             else main lexbuf
}
