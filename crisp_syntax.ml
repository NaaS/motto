(*
   AST spec for Crisp
   Nik Sultana, Cambridge University Computer Lab, January 2015
   (based on prototypical Matron language from last November)
*)

open General
open Crisp_type_annotation

(*NOTE currently we don't allow programmers to have a non-zero program-level
  indentation. Cannot think of a reason why this policy is a bad thing.*)
let min_indentation = 0
(*When we indent, we indent by this amount of space*)
let indentation = 2

let default_use_mixfix_lists = true;;

(*FIXME could generalise to abstract names*)
type type_name = string
(*NOTE this language is first-order, and functions are not values.*)
type value_name = string
type function_name = string
type decorator_name = string
type label = string
type univ_type = string (*FIXME hack*)

type dependency_index = string

type channel_name = string

(*NOTE cannot use sets instead of lists for records since order
       might matter (certainly if the type needs to be serialised).*)
(*Labels are used to implement labelled variants over disjoint unions.*)
type type_value =
  (*A reference to a type defined earlier in the program*)
  (*NOTE the type annotation of a user-defined type is implicit
         in the annotations of its contents.*)
  | UserDefinedType of label option * type_name
  | String of label option * type_annotation
  | Integer of label option * type_annotation
  | Boolean of label option * type_annotation
  | RecordType of label option * type_value list * type_annotation
  | Disjoint_Union of label option * type_value list (*FIXME type annotation?*)
  | List of label option * type_value *
            dependency_index option * type_annotation
  | Empty
  | IPv4Address of label option
  (*We send records, not tuples, over the wire, so tuples
    don't need type annotations.*)
  | Tuple of label option * type_value list
  | Dictionary of label option * type_value (*FIXME should also specify type of
                                                    the index, as well as the contained
                                                    type?*)
  | Reference of label option * type_value
  | Undefined
    (*NOTE ChanType should not be contained in any other types -- lists,
           variants, etc*)
  | ChanType of channel_type
and channel_type =
  | ChannelSingle of type_value * type_value
  | ChannelArray of type_value * type_value * dependency_index option
and channel = Channel of channel_type * channel_name

let flick_unit_type = Tuple (None, [])

let rec type_value_to_string ?summary_types:(summary_types : bool = false) mixfix_lists ending_newline indent ty_value =
  let endline = if ending_newline then "\n" else "" in
  let use_mixfix_list_syntax_for = function
    | UserDefinedType _
    | String _
    | Integer _
    | Boolean _
    | Tuple _
    | List _ -> true
    | _ -> false
  in match ty_value with
  | UserDefinedType (label, type_name) ->
      opt_string (indn indent) label " : " ^ "type " ^ type_name ^ endline
  | String (label, ann) ->
    opt_string (indn indent) label " : " ^ "string" ^
    ann_string indent indentation ann ^ endline
  | Integer (label, ann) ->
    opt_string (indn indent) label " : " ^ "integer" ^
    ann_string indent indentation ann ^ endline
  | Boolean (label, ann) ->
    opt_string (indn indent) label " : " ^ "boolean" ^
    ann_string indent indentation ann ^ endline
  | RecordType (label, tys, ann) ->
    opt_string (indn indent) label " : " ^ "record" ^
    if summary_types then "" else
      ann_string indent indentation ann ^  "\n" ^
      mk_block (indent + indentation) (type_value_to_string mixfix_lists ending_newline) tys
  | Disjoint_Union (label, tys) ->
    opt_string (indn indent) label " : " ^ "variant" ^ "\n" ^
    if summary_types then "" else
      mk_block (indent + indentation) (type_value_to_string mixfix_lists ending_newline) tys
  | List (label, ty, dep_idx_opt, ann) ->
    let s =
      if mixfix_lists && use_mixfix_list_syntax_for ty(*FIXME possible bug: i
                                                        think this should be
                                                        ty_value not ty*) then
        opt_string (indn indent) label " : " ^ "[" ^
         type_value_to_string mixfix_lists false indent ty ^
           "]" ^ opt_string "{" dep_idx_opt "}"
      else
        opt_string (indn indent) label " : " ^ "list" ^
         opt_string "{" dep_idx_opt "}" ^ " " ^
        type_value_to_string mixfix_lists ending_newline indent ty
    in s ^ ann_string indent indentation ann ^ endline
  | Empty -> "-" ^ endline
  | IPv4Address label ->
      opt_string (indn indent) label " : " ^ "ipv4_address" ^ endline
  | Tuple (label, tys) ->
    if use_mixfix_list_syntax_for ty_value then
      opt_string (indn indent) label " : " ^ "<" ^
       String.concat " * " (List.map (type_value_to_string mixfix_lists false 0) tys) ^
        ">" ^ endline
    else
      opt_string (indn indent) label " : " ^ "tuple (" ^
       String.concat ", " (List.map (type_value_to_string mixfix_lists false 0) tys) ^
        ")" ^ endline
  | Dictionary (label, ty) ->
      opt_string (indn indent) label " : " ^ "dictionary " ^
       type_value_to_string mixfix_lists false 0 ty ^
        endline
  | Reference (label, ty) ->
      opt_string (indn indent) label " : " ^ "ref " ^
       type_value_to_string mixfix_lists false 0 ty ^
        endline
  | Undefined -> "undefined"
  | ChanType ct -> "channel " ^ channel_type_to_string ct
and channel_type_to_string = function
  | ChannelSingle (type_value1, type_value2) ->
    type_value_to_string default_use_mixfix_lists false 0 type_value1 ^ "/" ^
    type_value_to_string default_use_mixfix_lists false 0 type_value2
  | ChannelArray (type_value1, type_value2, dep_idx_opt) ->
    "[" ^ type_value_to_string default_use_mixfix_lists false 0 type_value1 ^ "/" ^
    type_value_to_string default_use_mixfix_lists false 0 type_value2 ^ "]" ^
    opt_string "{" dep_idx_opt "}"
let channel_to_string (Channel (channel_type, channel_name)) =
  channel_type_to_string channel_type ^ " " ^ channel_name

let undefined_ty ty = ty = Undefined

type typing = value_name * type_value option

(*NOTE Currently only this kind of decorator parameter is supported: type*)
type decorator_param =
  | TypeDecorator of type_value

type decorator =
  {dec_name : decorator_name;
   dec_params : decorator_param list}

type process_type =
  ProcessType of dependency_index list * (channel list * type_value list)
let process_type_to_string (ProcessType (dvars, (chans, params))) =
  let deps =
    if dvars = [] then ""
    else
      "{" ^ String.concat ", " dvars ^ "} => " in
  let params_s =
    if params = [] then ""
    else
      "; " ^
      String.concat ", " (List.map (type_value_to_string default_use_mixfix_lists false 0) params)
  in deps ^ "(" ^ String.concat ", " (List.map channel_to_string chans) ^
    params_s ^ ")"
;;

type function_domtype = FunDomType of channel list * type_value list
let function_domtype_to_string (FunDomType (chans, params)) =
  let chan_params =
    if List.length chans > 0 then
      String.concat ", " (List.map channel_to_string chans) ^ "; "
    else "" in
  "(" ^ chan_params ^
    String.concat ", " (List.map (type_value_to_string default_use_mixfix_lists false 0) params) ^
    ")"
;;
type function_rettype = FunRetType of type_value list
let function_rettype_to_string (FunRetType tys) =
  "(" ^ String.concat ", " (List.map (type_value_to_string default_use_mixfix_lists false 0) tys) ^ ")"
;;
type function_type = FunType of function_domtype * function_rettype
let function_type_to_string (FunType (fd, fr)) =
  function_domtype_to_string fd ^ " -> " ^ function_rettype_to_string fr
;;

type integer = int (*FIXME precision*)

type fun_arg =
  | Exp of expression
  | Named of label * expression

and expression =
  | Variable of label

  (*Boolean expressions*)
  | True
  | False
  | And of expression * expression
  | Or of expression * expression
  | Not of expression

  (*Definable over arbitrary types of expressions*)
  | Equals of expression * expression

  (*Definable over arithmetic expressions*)
  | GreaterThan of expression * expression
  | LessThan of expression * expression

  (*Arithmetic expressions*)
  (*FIXME no floats or unary minus yet*)
  | Int of integer
  | Plus of expression * expression
  | Minus of expression * expression
  | Times of expression * expression
  | Mod of expression * expression
  | Quotient of expression * expression
  | Abs of expression

(*FIXME include concat, substring, etc
  | Str of string
  | To_Str of expression
*)

  (*Native representation of an IPv4 address*)
  | IPv4_address of (int * int * int * int)
  (*Integer to IP address*)
  | Int_to_address of expression
  (*IP address to integer*)
  | Address_to_int of expression
(*Other ideas:
  - Projecting an octet from an IP address, and updating
  - 4-to-6
*)

  | EmptyList
  | ConsList of expression * expression
  | AppendList of expression * expression

  | TupleValue of expression list

  | Seq of expression * expression
  | ITE of expression * expression * expression option
  | LocalDef of typing * expression (*def value_name : type = expression*)
  | Update of value_name * expression (*value_name := expression*)
  (*value_name[idx] := expression*)
  | UpdateIndexable of value_name * expression * expression

  (*This work for both tuples and records.*)
  | RecordProjection of expression * label

  | Functor_App of function_name * fun_arg list

  | Record of (label * expression) list
  | RecordUpdate of (expression * (label * expression))

  (*Case elimination on variants; formation of variant
    instances will look like function application in the
    language, therefore it doesn't require special syntax.*)
  | CaseOf of expression * (expression * expression) list

  (*The first parameter could be generalised to an expression,
    but I don't think we need that expressiveness at the moment.
    Also, the second parameter could be specialised to a natural
    number -- we might go for that for the moment.*)
  | IndexableProjection of label * expression

  | IntegerRange of expression * expression
  | Map of label * expression * expression * bool
  | Iterate of label * expression *
               (label * expression) option *
               expression * bool

  (*Channel operations. Can be overloaded to, say, send values
    on a channel, or to first obtain values from a channel then send it to
    another.*)
  | Send of expression * expression
  | Receive of expression * expression
  (*Send and receive between two channels*)
  | Exchange of expression * expression

  | Str of string

let rec expression_to_string indent = function
  | Variable value_name -> indn indent ^ value_name
  | Seq (e1, e2) ->
    expression_to_string indent e1 ^ "\n" ^
    expression_to_string indent e2

  | True -> indn indent ^ "True"
  | False -> indn indent ^ "False"
  | And (b1, b2) ->
    indn indent ^ "((" ^ expression_to_string 0 b1 ^ ") and (" ^
    expression_to_string 0 b2 ^ "))"
  | Or (b1, b2) ->
    indn indent ^ "((" ^ expression_to_string 0 b1 ^ ") or (" ^
    expression_to_string 0 b2 ^ "))"
  | Not b' ->
    indn indent ^ "(not " ^ expression_to_string 0 b' ^ ")"

  | ITE (be, e1, e2_opt) ->
    let e2_s =
      match e2_opt with
      | None -> ""
      | Some e2 ->
        "\n" ^
        indn indent ^ "else:\n" ^
        expression_to_string (indent + indentation) e2 in
    indn indent ^ "if " ^
    expression_to_string 0 be ^ ":\n" ^
    expression_to_string (indent + indentation) e1 ^
    e2_s

  | Update (value_name, expression) ->
    (*NOTE for proper pretty-printing we can use width-senstitive generation of
           code blocks, as is standard. Currently this approach is crude, to get
           going.*)
    indn indent ^ value_name ^ " := " ^ expression_to_string 0 expression
  | UpdateIndexable (value_name, idx, expression) ->
    indn indent ^ value_name ^ "[" ^
    expression_to_string 0 idx ^ "]" ^
    " := " ^ expression_to_string 0 expression

  | LocalDef ((v, ty_opt), e) ->
    let ty_s =
      match ty_opt with
      | None -> ""
      | Some ty -> " : " ^
        type_value_to_string default_use_mixfix_lists false 0 ty in
    indn indent ^ "let " ^ v ^ ty_s ^ " =\n" ^
    expression_to_string (indent + indentation) e

  | Equals (e1, e2) ->
    indn indent ^ "((" ^ expression_to_string 0 e1 ^ ") = (" ^
    expression_to_string 0 e2 ^ "))"

  | GreaterThan (a1, a2) ->
    indn indent ^ "((" ^ expression_to_string 0 a1 ^ ") > (" ^
    expression_to_string 0 a2 ^ "))"
  | LessThan (a1, a2) ->
    indn indent ^ "((" ^ expression_to_string 0 a1 ^ ") < (" ^
    expression_to_string 0 a2 ^ "))"

  | Int n -> indn indent ^ string_of_int n
  | Plus (a1, a2) ->
    indn indent ^ "((" ^ expression_to_string 0 a1 ^ ") + (" ^
    expression_to_string 0 a2 ^ "))"
  | Minus (a1, a2) ->
    indn indent ^ "((" ^ expression_to_string 0 a1 ^ ") - (" ^
    expression_to_string 0 a2 ^ "))"
  | Times (a1, a2) ->
    indn indent ^ "((" ^ expression_to_string 0 a1 ^ ") * (" ^
    expression_to_string 0 a2 ^ "))"
  | Mod (a1, a2) ->
    indn indent ^ "((" ^ expression_to_string 0 a1 ^ ") mod (" ^
    expression_to_string 0 a2 ^ "))"
  | Quotient (a1, a2) ->
    indn indent ^ "((" ^ expression_to_string 0 a1 ^ ") / (" ^
    expression_to_string 0 a2 ^ "))"
  | Abs a ->
    indn indent ^ "abs (" ^ expression_to_string 0 a ^ ")"

  | IPv4_address (o1, o2, o3, o4) ->
    indn indent ^ string_of_int o1 ^ "." ^ string_of_int o2 ^ "." ^
    string_of_int o3 ^ "." ^ string_of_int o4
  | Address_to_int e ->
    indn indent ^ "address_to_int (" ^ expression_to_string 0 e ^ ")"
  | Int_to_address e ->
    indn indent ^ "int_to_address (" ^ expression_to_string 0 e ^ ")"

  | EmptyList ->
    indn indent ^ "[]"
  | ConsList (x, xs) ->
    indn indent ^ "((" ^ expression_to_string 0 x ^ ") :: (" ^
    expression_to_string 0 xs ^ "))"
  (*FIXME could use nicer pretty-printing*)
  | AppendList (xs, ys) ->
    indn indent ^ "((" ^ expression_to_string 0 xs ^ ") @ (" ^
    expression_to_string 0 ys ^ "))"

  | TupleValue xs ->
    indn indent ^ "<" ^
      String.concat ", " (List.map (expression_to_string 0) xs) ^ ">"

  | RecordProjection (e, l) ->
    indn indent ^ expression_to_string 0 e ^ "." ^ l

  | Functor_App (f, es) ->
    let fun_arg_to_string = function
      | Exp e -> expression_to_string 0 e
      | Named (l, e) -> l ^ " <- " ^ expression_to_string 0 e
    in
    indn indent ^ f ^ " (" ^
    String.concat ", " (List.map fun_arg_to_string es) ^ ")"

  | Record entries ->
    let entry_to_string (l, e) =
      l ^ " = " ^ expression_to_string 0 e in
    indn indent ^ "{" ^
    String.concat ", " (List.map entry_to_string entries) ^ "}"
  | RecordUpdate (r, ((l, e) as entry)) ->
    let entry_to_string (l, e) =
      l ^ " = " ^ expression_to_string 0 e in
    indn indent ^ expression_to_string 0 r ^ " with " ^
    entry_to_string entry

  | CaseOf (e, matches) ->
    let match_to_string indent (guard, body) =
      indn indent ^ expression_to_string 0 guard ^ ":\n" ^
      indn (indent + indentation) ^ expression_to_string 0 body ^ "\n"
    in
      indn indent ^ "switch " ^ expression_to_string 0 e ^ ":\n" ^
      mk_block (indent + indentation) match_to_string  matches

  | IndexableProjection (v, idx) ->
    indn indent ^ v ^ "[" ^
      expression_to_string 0 idx ^ "]"

  | IntegerRange (e1, e2) ->
    expression_to_string indent e1 ^ ".." ^
     expression_to_string indent e2
  | Map (v, l, body, unordered) ->
    let unordered_s = if unordered then "unordered " else "" in
    indn indent ^ "map " ^ v ^ " in " ^ unordered_s ^
      expression_to_string 0 l ^ ":\n" ^
      expression_to_string (indent + indentation) body
  | Iterate (v, l, acc, body, unordered) ->
    let unordered_s = if unordered then "unordered " else "" in
    let acc_s = match acc with
      | None -> ":\n"
      | Some (acc_v, acc_e) -> "\n" ^
        indn indent ^ "initially " ^ acc_v ^
          " = " ^ expression_to_string 0 acc_e ^ ":\n"
    in
      indn indent ^ "for " ^ v ^ " in " ^ unordered_s ^
        expression_to_string 0 l ^ acc_s ^
        expression_to_string (indent + indentation) body

  | Send (e1, e2) ->
    expression_to_string indent e1 ^ " => " ^
     expression_to_string 0 e2
  | Receive (e1, e2) ->
    expression_to_string indent e1 ^ " <= " ^
     expression_to_string 0 e2
  | Exchange (e1, e2) ->
    expression_to_string indent e1 ^ " <=> " ^
     expression_to_string 0 e2

  | Str s -> "\"" ^ s ^ "\""

type process_name = string

type state_decl =
  | LocalState of label * type_value option * expression
  | GlobalState of label * type_value option * expression
let state_decl_to_string indent state_decl =
  let decl_state (kind, label, type_value_opt, expression) =
    let ty_s =
      match type_value_opt with
      | None -> ""
      | Some ty ->
        " : " ^ type_value_to_string true false 0 ty
    in indn indent ^ kind ^ " " ^ label ^ ty_s ^ " := " ^
       expression_to_string 0 expression
  in match state_decl with
  | LocalState (label, type_value_opt, expression) ->
    decl_state ("local", label, type_value_opt, expression)
  | GlobalState (label, type_value_opt, expression) ->
    decl_state ("global", label, type_value_opt, expression)

type excepts_decl = label * expression
let excepts_decl_to_string indent (label, e) =
  indn indent ^ "except " ^ label ^ " : " ^ expression_to_string 0 e

type process_body =
    ProcessBody of state_decl list * expression * excepts_decl list
let process_body_to_string indent (ProcessBody (st_decls, e, exc_decls)) =
  let st_decls_s =
    List.map (state_decl_to_string indent) st_decls
    |> String.concat "\n" in
  let e_s = expression_to_string indent e in
  let exc_decls_s =
    List.map (excepts_decl_to_string indent) exc_decls
    |> String.concat "\n" in
  (if st_decls = [] then "" else st_decls_s ^ "\n") ^
  e_s ^
  (if exc_decls = [] then "" else "\n" ^ exc_decls_s)

type process =
  { process_name : process_name;
    process_type : process_type;
    process_body : process_body;
  }

type ty_decl =
  {type_name : type_name;
   type_value : type_value}
let ty_decl_to_string {type_name; type_value} =
  type_name ^ ": " ^ type_value_to_string default_use_mixfix_lists true min_indentation type_value
type fn_decl =
  {fn_name : function_name;
   fn_params : function_type;
   fn_body : process_body}

(*Top-level declarations. We cannot define types or functions within functions*)
type toplevel_decl =
  | Type of ty_decl
  | Function of fn_decl
  | Process of process
  | Include of string
let toplevel_decl_to_string = function
  | Type ty_decl -> "type " ^ ty_decl_to_string ty_decl
  | Process process ->
    "proc " ^ process.process_name ^ " : " ^ process_type_to_string process.process_type ^
     "\n" ^ process_body_to_string indentation process.process_body
  | Function fn_decl ->
    "fun " ^ fn_decl.fn_name ^ " : " ^ function_type_to_string fn_decl.fn_params ^
     "\n" ^ process_body_to_string indentation fn_decl.fn_body
  | Include s ->
    "include \"" ^ s ^ "\""

type program = toplevel_decl list
let program_to_string (p : program) =
  List.map toplevel_decl_to_string p
  |> String.concat "\n"

type source_file_contents =
  | Program of program
  | Expression of expression
  | Empty
let source_file_contents_to_string (p : source_file_contents) =
  match p with
  | Program p -> program_to_string p
  | Expression e ->
    "(| " ^ expression_to_string min_indentation e ^ " |)"
  | Empty -> "(Empty)"
