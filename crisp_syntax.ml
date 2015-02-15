(*
   AST spec for Crisp
   Nik Sultana, Cambridge University Computer Lab, January 2015
   (based on prototypical Matron language from last November)
*)

(*NOTE currently we don't allow programmers to have a non-zero program-level
  indentation. Cannot think of a reason why this policy is a bad thing.*)
let min_indentation = 0
(*When we indent, we indent by this amount of space*)
let indentation = 2

(*FIXME could generalise to abstract names*)
type type_name = string
(*NOTE this language is first-order, and functions are not values.*)
type value_name = string
type function_name = string
type decorator_name = string
type label = string
type univ_type = string (*FIXME hack*)

let opt_string (prefix : string) (s : string option) (suffix : string) : string =
  match s with
  | None -> ""
  | Some s' -> prefix ^ s' ^ suffix
;;
let replicate (s : string) (count : int) =
  assert (count > -1);
  let rec replicate' (acc : string) (i : int) =
    if i = 0 then acc
    else replicate' (acc ^ s) (i - 1)
  in
    replicate' "" count
;;
let indn (indent : int) : string =
  replicate " " indent
;;
let mk_block (indent : int) (f : int -> 'a -> string) (l : 'a list) : string =
  List.fold_right (fun x already ->
    already ^ f indent x) l ""
;;

type dependency_index = string

(*Labels are used to implement labelled variants over disjoint unions.*)
type type_value =
  (*FIXME include delimiters for string and list*)
  (*A reference to a type defined earlier in the program*)
  | UserDefinedType of label option * type_name
  | String of label option
  | Integer of label option
  | Boolean of label option
  | Record of label option * type_value list
  | Disjoint_Union of label option * type_value list
  | Unit of label option
  | List of label option * type_value * dependency_index option
  | Empty
;;

let rec type_value_to_string mixfix_lists ending_newline indent ty_value =
  let endline = if ending_newline then "\n" else "" in
  let use_mixfix_list_syntax_for = function
    | UserDefinedType _
    | String _
    | Integer _
    | Boolean _
    | Unit _
    | List _ -> true
    | _ -> false
  in match ty_value with
  | UserDefinedType (label, type_name) ->
      opt_string (indn indent) label " : " ^ "type " ^ type_name ^ endline
  | String label ->
      opt_string (indn indent) label " : " ^ "string" ^ endline
  | Integer label ->
      opt_string (indn indent) label " : " ^ "integer" ^ endline
  | Boolean label ->
      opt_string (indn indent) label " : " ^ "boolean" ^ endline
  | Record (label, tys) ->
      opt_string (indn indent) label " : " ^ "record" ^ "\n" ^
      mk_block (indent + indentation) (type_value_to_string mixfix_lists ending_newline) tys
  | Disjoint_Union (label, tys) ->
      opt_string (indn indent) label " : " ^ "variant" ^ "\n" ^
      mk_block (indent + indentation) (type_value_to_string mixfix_lists ending_newline) tys
  | Unit label ->
      opt_string (indn indent) label " : " ^ "unit" ^ endline
  | List (label, ty, dep_idx_opt) ->
    if mixfix_lists && use_mixfix_list_syntax_for ty then
      opt_string (indn indent) label " : " ^ "[" ^
       type_value_to_string mixfix_lists false indent ty ^
         "]" ^ opt_string "{" dep_idx_opt "}" ^ endline
    else
      opt_string (indn indent) label " : " ^ "list" ^
       opt_string "{" dep_idx_opt "}" ^ " " ^
        type_value_to_string mixfix_lists ending_newline indent ty
  | Empty -> "-"
;;

type typing = value_name * type_value

(*NOTE Currently only this kind of decorator parameter is supported: type*)
type decorator_param =
  | TypeDecorator of type_value

type decorator =
  {dec_name : decorator_name;
   dec_params : decorator_param list}

let default_use_mixfix_lists = true;;

type channel_type =
  | ChannelSingle of type_value * type_value
  | ChannelArray of type_value * type_value * dependency_index option
let channel_type_to_string = function
  | ChannelSingle (type_value1, type_value2) ->
    type_value_to_string default_use_mixfix_lists false 0 type_value1 ^ "/" ^
    type_value_to_string default_use_mixfix_lists false 0 type_value2
  | ChannelArray (type_value1, type_value2, dep_idx_opt) ->
    "[" ^ type_value_to_string default_use_mixfix_lists false 0 type_value1 ^ "/" ^
    type_value_to_string default_use_mixfix_lists false 0 type_value2 ^ "]" ^
    opt_string "{" dep_idx_opt "}"
type channel_name = string
type channel = Channel of channel_type * channel_name
let channel_to_string (Channel (channel_type, channel_name)) =
  channel_type_to_string channel_type ^ " " ^ channel_name

let inter (mid : string) (ss : string list) =
  List.fold_right (fun x s ->
    if s = "" then x
    else x ^ mid ^ s) ss ""
;;

type process_type = ProcessType of dependency_index list * channel list
let process_type_to_string (ProcessType (dvars, chans)) =
  let deps =
    if dvars = [] then ""
    else
      "{" ^ inter ", " dvars ^ "} => "
  in deps ^ "(" ^ inter ", " (List.map channel_to_string chans) ^ ")"
;;

type function_domtype = FunDomType of channel list * type_value list
let function_domtype_to_string (FunDomType (chans, params)) =
  let chan_params =
    if List.length chans > 0 then
      inter ", " (List.map channel_to_string chans) ^ "; "
    else "" in
  "(" ^ chan_params ^
    inter ", " (List.map (type_value_to_string default_use_mixfix_lists false 0) params) ^
    ")"
;;
type function_rettype = FunRetType of type_value list
let function_rettype_to_string (FunRetType tys) =
  "(" ^ inter ", " (List.map (type_value_to_string default_use_mixfix_lists false 0) tys) ^ ")"
;;
type function_type = FunType of function_domtype * function_rettype
let function_type_to_string (FunType (fd, fr)) =
  function_domtype_to_string fd ^ " -> " ^ function_rettype_to_string fr
;;

type bool_exp =
  | True
  | False
  | Bool_Val of value_name
  | And of bool_exp * bool_exp
  | Or of bool_exp * bool_exp
  | Not of bool_exp
(*FIXME bracketing sucks*)
let rec bool_exp_to_string indent = function
  | True -> indn indent ^ "True"
  | False -> indn indent ^ "False"
  | Bool_Val value_name -> indn indent ^ value_name
  | And (b1, b2) ->
    indn indent ^ "((" ^ bool_exp_to_string 0 b1 ^ ") and (" ^
    bool_exp_to_string 0 b2 ^ "))"
  | Or (b1, b2) ->
    indn indent ^ "((" ^ bool_exp_to_string 0 b1 ^ ") or (" ^
    bool_exp_to_string 0 b2 ^ "))"
  | Not b' ->
    indn indent ^ "(not " ^ bool_exp_to_string 0 b' ^ ")"

type integer = int (*FIXME precision*)

(*FIXME no division, nor coercions into floats yet*)
type arith_exp =
  | Int of integer
  | Plus of integer * integer
  | Minus of integer * integer
  | Times of integer * integer

(*FIXME incomplete -- should the other features, e.g., concat, substring, etc,
  be implemented in a library?*)
type str_exp =
  | Str of string

type carts =
  {value_type : type_value;
   label : label;
   cart_value : univ_type}

type co_carts =
  { du_type : type_value;
    du_value : carts}

type rec_exp =
  | Rec of carts list
  | Rec_Projection of (*rec_label*) label * (*rec_val*) value_name

type du_exp =
  | Inj of co_carts

(*FIXME this defn is from Matron -- need to remove some parts (e.g., yield) and
        rename it to "expression" to better fit Flick.*)
(*FIXME should restrict sub-expressions to specific classes (e.g., bool_exp), or
be liberal instead? i.e., allow them to be function_body?*)
type function_body =
  | Unity
  | BExp of bool_exp
  | AExp of arith_exp
  | StExp of str_exp
  | RecExp of rec_exp
  | VariantExp of du_exp (*FIXME make naming more consistent*)
  (*NOTE the syntax is pretty powerfuli -- it might not be a loss if we
    restricted function arguments (cf Function_Call) to be values, rather than
    expressions.*)
  | Function_Call of function_name * function_body list
  | Seq of function_body * function_body
  | Yield
  | ITE of bool_exp * function_body * function_body
  | Iterate of function_body * function_body * function_body
  | LocalDef of typing * function_body (*def value_name : type = function_body*)
  | Update of value_name * function_body (*value_name := function_body*)
let function_body_to_string indent = function
  (*FIXME incomplete*)
  | Unity ->
    indn indent ^ "<>"
  | BExp b -> bool_exp_to_string indent b
    (*FIXME for remainder of this could emulate how blocks are printed*)
  | _ -> failwith "Unsupported"

(*TODO: should be similar to function_body, but also allows calls to carry_ons*)
type carry_on_body = unit

type ty_decl =
  {type_name : type_name;
   type_value : type_value}
let ty_decl_to_string {type_name; type_value} =
  type_name ^ ": " ^ type_value_to_string default_use_mixfix_lists true min_indentation type_value
type co_decl =
  {decorator : decorator option;
   co_name : function_name;
   co_params : type_value list;
   co_body : carry_on_body}
type fn_decl =
  {fn_name : function_name;
   fn_params : function_type;
   fn_body : function_body}

type process_name = string

type state_decl =
  | LocalState of label * type_value option * function_body
  | GlobalState of label * type_value option * function_body
let state_decl_to_string indent state_decl =
  let decl_state (kind, label, type_value_opt, expression) =
    let ty_s =
      match type_value_opt with
      | None -> ""
      | Some ty ->
        " : " ^ type_value_to_string true false 0 ty
    in indn indent ^ kind ^ " " ^ label ^ ty_s ^ " := " ^
       function_body_to_string 0 expression
  in match state_decl with
  | LocalState (label, type_value_opt, expression) ->
    decl_state ("local", label, type_value_opt, expression)
  | GlobalState (label, type_value_opt, expression) ->
    decl_state ("global", label, type_value_opt, expression)

type excepts_decl = label * function_body
let excepts_decl_to_string indent (label, e) =
  indn indent ^ "except " ^ label ^ " : " ^ function_body_to_string 0 e

type process_body =
    ProcessBody of state_decl list * function_body * excepts_decl list
let process_body_to_string indent (ProcessBody (st_decls, e, exc_decls)) =
  let st_decls_s =
    List.map (state_decl_to_string indent) st_decls
    |> inter "\n" in
  let e_s = function_body_to_string indent e in
  let exc_decls_s =
    List.map (excepts_decl_to_string indent) exc_decls
    |> inter "\n" in
  st_decls_s ^ e_s ^ exc_decls_s

(*Top-level declarations. We cannot define types or functions within functions*)
type toplevel_decl =
  | Type of ty_decl
  | Carry_On of co_decl
  | Function of fn_decl
  | Process of process_name * process_type * process_body
let toplevel_decl_to_string = function
  | Type ty_decl -> "type " ^ ty_decl_to_string ty_decl
  | Process (process_name, process_type, process_body) ->
    "proc " ^ process_name ^ " : " ^ process_type_to_string process_type ^
     "\n" ^ process_body_to_string indentation process_body
  | Function fn_decl ->
    "fun " ^ fn_decl.fn_name ^ " : " ^ function_type_to_string fn_decl.fn_params ^
     "\n" ^ function_body_to_string indentation fn_decl.fn_body
  | _ -> failwith "Unsupported"

type program = toplevel_decl list
let program_to_string (p : program) =
  List.map toplevel_decl_to_string p
  |> inter "\n"
