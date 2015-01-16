{
open Lexing
open Crisp_parser

let scope_stack : int Stack.t =
  Stack.create ()
;;
Stack.push Crisp_syntax.min_indentation scope_stack;;

let test_indentation indentation lexbuf =
  assert (not (Stack.is_empty scope_stack)); (*There should always be at least
                                               one element in the stack: 0*)
  let next_line () =
    (*this function was adapted from
      https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html*)
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      } in
  (*Count how many scopes we've moved down (out of).*)
  let rec undented_scopes (offset : int) =
    if Stack.top scope_stack = indentation then
      offset
    else if Stack.top scope_stack < indentation then
      failwith "Undershot the scope?"
    else
      begin
        Stack.pop scope_stack;
        undented_scopes (offset + 1)
      end in
  let prev = Stack.top scope_stack in
    if indentation > prev then
      begin
        Stack.push indentation scope_stack;
        next_line ();
        INDENT
      end
    else if indentation = prev then
      begin
        next_line ();
        NL
      end
    else
      begin
        assert (indentation < prev);
        next_line ();
        UNDENTN (undented_scopes Crisp_syntax.min_indentation)
      end
}

(*NOTE tabs are not recognised, because they suck. They also make it more
  difficult to measure indentation when mixed with spaces..*)
let ws = ' '+
let comment = '#'[^'\n']*

(*NOTE currently only Unix-style newline is supported, because it's simpler.*)
let nl = '\n'

rule main = parse
  | comment {main lexbuf}
  | nl ' '* comment {main lexbuf}
  | nl (ws as spaces)
      {test_indentation (String.length spaces) lexbuf}
  | "type" {TYPE}
  | "integer" {TYPE_INTEGER}
  | "string" {TYPE_STRING}
  | "boolean" {TYPE_BOOLEAN}
  | "record" {TYPE_RECORD}
  | "variant" {TYPE_VARIANT}
  | "unit" {TYPE_UNIT}
  | "list" {TYPE_LIST}
  | ":" {COLON}
  | "proc" {PROC}
  | "/" {SLASH}
  | "[" {LEFT_S_BRACKET}
  | "]" {RIGHT_S_BRACKET}
  | "(" {LEFT_R_BRACKET}
  | ")" {RIGHT_R_BRACKET}
  | "{" {LEFT_C_BRACKET}
  | "}" {RIGHT_C_BRACKET}
  | "," {COMMA}
  | "=>" {ARR_RIGHT}
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id {IDENTIFIER id}
  | nl {test_indentation Crisp_syntax.min_indentation lexbuf}
  | ws {main lexbuf}
  | eof {EOF}

