(*
  Parser interface for Flick
  Nik Sultana, Cambridge University Computer Lab, January 2015

  Parts of this module is based on explanation & code given at
  https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open Lexing
open Crisp_syntax
open Debug

(*NOTE this code depends on side-effects to expand tokens, to deal with the
  constraint that the lexer only emits one token at a time.*)
let token_q_expand : Crisp_parser.token Queue.t = Queue.create ()
let token_q_filter : Crisp_parser.token Queue.t = Queue.create ()

let enqueue_token token_q (i : int) (token : Crisp_parser.token) =
  let cnt = ref i in
  while !cnt > 0 do
    Queue.add token token_q;
    cnt := !cnt - 1
  done
let token_stream_processor (token_q : Crisp_parser.token Queue.t)
  (wrapper_lexer : Crisp_parser.token Queue.t ->
   (Lexing.lexbuf -> Crisp_parser.token) ->
   Lexing.lexbuf -> Crisp_parser.token)
  (lexer : Lexing.lexbuf -> Crisp_parser.token)
  (lexbuf : Lexing.lexbuf) : Crisp_parser.token =
  if Queue.is_empty token_q then
    wrapper_lexer token_q lexer lexbuf
  else
    Queue.take token_q

(*turns NL,NL into NL
  and NL,UNDENT into UNDENT*)
let filter_redundant_newlines
  (lexer : Lexing.lexbuf -> Crisp_parser.token)
  (lexbuf : Lexing.lexbuf) : Crisp_parser.token =
  let token_q = token_q_filter in
  let wrapper_lexer token_q lexer lexbuf =
    let munch_newlines () =
      let token_r = ref (lexer lexbuf) in
      while !token_r = Crisp_parser.NL do
        token_r := lexer lexbuf
      done;
      !token_r in
    match lexer lexbuf with
    | Crisp_parser.NL as token ->
      (*Continue munching tokens until we reach something that's not NL.
        If that something is UNDENT, then forget all the NLs.
        If it's not UNDENT, then emit a single NL.*)
        let next_non_nl_token = munch_newlines () in
        if next_non_nl_token = Crisp_parser.UNDENT then
          Crisp_parser.UNDENT
        else
          begin
            enqueue_token token_q 1 next_non_nl_token;
            token
          end
      | token -> token
  in token_stream_processor token_q wrapper_lexer lexer lexbuf

let expand_macro_tokens
  (lexer : Lexing.lexbuf -> Crisp_parser.token)
  (lexbuf : Lexing.lexbuf) : Crisp_parser.token =
  let token_q = token_q_expand in
  let wrapper_lexer token_q lexer lexbuf =
    let expand_macro (times : int) (token : Crisp_parser.token)
          (trailing_tokens : Crisp_parser.token list) =
        assert (times > -1); (*we can have UNINDENTN 0 times, in case we just had
                               an \n*)
        let insert_trailing_tokens () =
          List.iter
            (fun tok -> Queue.add tok token_q)
            trailing_tokens in
        if times > 0 then
          begin
            enqueue_token token_q (times - 1) token;
            insert_trailing_tokens ();
            token
          end
        else
          Crisp_parser.NL (*if we have 0 undents then it still means that we had a
                            newline.*)
    in match lexer lexbuf with
      | Crisp_parser.UNDENTN (n, follow_on_tokens) ->
        let trailing_tokens =
          if follow_on_tokens <> [] then
            follow_on_tokens
          (*always have an NL following (one or more) UNDENTs. This allows us to
            parse nested records, for example, since the contents of the
            containing record must be separated by an NL.*)
          else [Crisp_parser.NL]
        in expand_macro n Crisp_parser.UNDENT trailing_tokens
      | token -> token
  in token_stream_processor token_q wrapper_lexer lexer lexbuf

(*FIXME error info should be inside the actual error, not lexbuf*)
let handle_lex_error ?(silent=false) ex lexbuf =
  match ex with
  | Failure _ | Crisp_lexer.Error ->
      if not silent then
        Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf
  | e -> raise e (*If we don't handle it here, re-raise it*)

let lex_with_error ?(silent=false) lexer lexbuf =
  try Some (lexer lexbuf)
  with
  | ex -> handle_lex_error ~silent ex lexbuf; None

let lex_step_with_error ?(silent=false) lexbuf =
  lex_with_error ~silent Crisp_lexer.main lexbuf


let parse_with_error ?(silent=false) lexbuf : Crisp_syntax.source_file_contents =
  let lexer = 
    Crisp_lexer.main
    |> expand_macro_tokens
    |> filter_redundant_newlines in
  let wrapped_lexer lexbuf =
    try lexer lexbuf with
    | ex ->
        (*This will re-raise any errors that aren't handled as a lexing error,
          so allow them to propogate since they probably shouldn't be
          presented as parsing errors. *)
        handle_lex_error ex lexbuf;
        (*wrap the lexing exception as a generic lexing exception
          since we have already handled it if we can, so that parsing fails.*)
        raise Crisp_lexer.Error
  in
  try
    Crisp_parser.source_file_contents wrapped_lexer lexbuf
  with 
  | Crisp_parser.Error ->
      if not silent then
        Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      Empty
  | Crisp_lexer.Error -> (* Lexing error - already handled *)
      Empty

let rec parse_and_print lexbuf =
  parse_with_error lexbuf
(*
  match parse_with_error lexbuf with
  | [] -> ()
  | value ->
    printf "%a\n" (*Json.output_value*) value;
    parse_and_print lexbuf
*)

let parse_file filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let result = parse_and_print lexbuf in
    close_in inx;
    result

let parse_string s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- lexbuf.lex_curr_p;
  parse_and_print lexbuf
