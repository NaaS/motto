{
open Lexing
open Crisp_parser

let scope_stack : int Stack.t =
  Stack.create ()
;;
let min_indentation = 0;;
(*NOTE currently we don't allow programmers to have a non-zero program-level
  indentation. Cannot think of a reason why this policy is a bad thing.*)
Stack.push min_indentation scope_stack;;

let test_indentation indentation =
  assert (not (Stack.is_empty scope_stack)); (*There should always be at least
                                               one element in the stack: 0*)
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
        INDENT
      end
    else if indentation = prev then
      begin
        NL
      end
    else
      begin
        assert (indentation < prev);
        UNDENTN (undented_scopes 0)
      end
}

(*NOTE tabs are not recognised, because they suck. They also make it more
  difficult to measure indentation when mixed with spaces..*)
let ws = ' '+

(*NOTE currently only Unix-style newline is supported, because it's simpler.*)
let nl = '\n'

rule main = parse
  | nl (ws as spaces)
      {test_indentation (String.length spaces)}
  | "type" {TYPE}
  | "integer" {TYPE_INTEGER}
  | "string" {TYPE_STRING}
  | "boolean" {TYPE_BOOLEAN}
  | "record" {TYPE_RECORD}
  | "variant" {TYPE_VARIANT}
  | ":" {COLON}
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id {IDENTIFIER id}
  | nl {test_indentation 0}
  | ws {main lexbuf}
  | eof {EOF}

