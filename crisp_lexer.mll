{
open Lexing
open Crisp_parser

let prev_ws_count : int option ref = ref None
let count_ws = ref true
}

(*NOTE tabs are not recognised, because they suck. They also make it more
  difficult to measure indentation when mixed with spaces..*)
let ws = ' '+

(*NOTE currently only Unix-style newline is supported, because it's simpler.*)
let nl = '\n'

rule main = parse
  | "type" {count_ws := false; TYPE}
  | "integer" {count_ws := false; TYPE_INTEGER}
  | "string" {count_ws := false; TYPE_STRING}
  | "boolean" {count_ws := false; TYPE_BOOLEAN}
  | "record" {count_ws := false; TYPE_RECORD}
  | "variant" {count_ws := false; TYPE_VARIANT}
  | ":" {count_ws := false; COLON}
  | ['a'-'z''_']+ as i {count_ws := false; IDENTIFIER i}
  | nl {count_ws := true; Lexing.new_line lexbuf; NL}
  | ws as spaces
       {if !count_ws then
           match !prev_ws_count with
           | None ->
               begin
                 prev_ws_count := Some (String.length spaces);
                 main lexbuf
               end
           | Some prev ->
               begin
                 prev_ws_count := Some (String.length spaces);
                 count_ws := false;
                 if (String.length spaces > prev) then
                   INDENT
                 else if (String.length spaces < prev) then
                   UNDENT
                 else
                   main lexbuf
               end
        else
          main lexbuf}
  | eof {EOF}

