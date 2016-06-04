{
(*
   Parser spec for Crisp
   Nik Sultana, Cambridge University Computer Lab, January 2015

   Target parser-generator: menhir 20140422

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open Lexing
open Crisp_parser

let scope_stack : int Stack.t =
  Stack.create ()
;;
Stack.push Crisp_syntax.min_indentation scope_stack;;

let test_indentation indentation follow_on_tokens lexbuf =
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
        ignore(Stack.pop scope_stack);
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
        next_line ();
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

rule main = parse
  | comment {main lexbuf}
  | nl+ ' '* comment {main lexbuf}
  | nl+ (ws as spaces)
      {test_indentation (String.length spaces) [] lexbuf}
  | "type" {TYPE}
  | "typed" {TYPED}
  | nl+"type" {test_indentation Crisp_syntax.min_indentation [TYPE] lexbuf}
  | "integer" {TYPE_INTEGER}
  | "string" {TYPE_STRING}
  | "boolean" {TYPE_BOOLEAN}
  | "record" {TYPE_RECORD}
  | "variant" {TYPE_VARIANT}
  | "list" {TYPE_LIST}
  | "tuple" {TYPE_TUPLE}
  | ":" {COLON}
  | "process" {test_indentation Crisp_syntax.min_indentation [PROC] lexbuf}
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
  | nl {NL}
  | ws {main lexbuf}
  | eof {test_indentation Crisp_syntax.min_indentation [EOF] lexbuf}
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
(*NOTE since functions are effectful, could replace proc with fun, and signal
  the entry point via some name -- e.g., main -- but then arbitrary functions
  would be able to register listeners for events.*)
  | "fun" {test_indentation Crisp_syntax.min_indentation [FUN] lexbuf}
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

  | "include" {INCLUDE}

  | "dictionary" {TYPE_DICTIONARY}
  | "ref" {TYPE_REF}

  | "can" {CAN}
  | "unsafe_cast" {UNSAFE_CAST}

  | "(|" {FAT_BRACKET_OPEN}
  | "(type|" {FAT_TYPE_BRACKET_OPEN}
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
