(*
   AST spec for Crisp
   Nik Sultana, Cambridge University Computer Lab, January 2015
   (based on prototypical Matron language from last November)
*)

(*FIXME could generalise to abstract names*)
type type_name = string
(*NOTE this language is first-order, and functions are not values.*)
type value_name = string
type function_name = string
type decorator_name = string
type label = string
type univ_type = string (*FIXME hack*)

let opt_string (prefix : string) (s : string option) : string =
  match s with
  | None -> ""
  | Some s' -> prefix ^ s'
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
    already ^ f indent x ^ "\n") l ""
;;

(*Labels are used to implement labelled variants over disjoint unions.*)
type type_value =
  | UserDefinedType of type_name (*A reference to a type defined earlier in the
                                 program*)
  | String of label option
  | Integer of label option
  | Boolean of label option
  | Record of label option * type_value list
  | Disjoint_Union of label option * type_value list
;;
let rec type_value_to_string indent = function
  | UserDefinedType type_name ->
      indn indent ^ type_name
  | String label ->
      "string" ^ opt_string " " label
  | Integer label ->
      "integer" ^ opt_string " " label
  | Boolean label ->
      "boolean" ^ opt_string " " label
  | Record (label, tys) ->
      "record" ^ opt_string " " label ^
      ":" ^ (*FIXME should be optional*)
      mk_block (indent + 2) type_value_to_string tys
  | Disjoint_Union _ -> failwith "Unsupported"
;;

type typing = value_name * type_value

(*NOTE Currently only this kind of decorator parameter is supported: type*)
type decorator_param =
  | TypeDecorator of type_value

type decorator =
  {dec_name : decorator_name;
   dec_params : decorator_param list}

type bool_exp =
  | True
  | False
  | Bool_Val of value_name

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

(*FIXME should restrict sub-expressions to specific classes (e.g., bool_exp), or
be liberal instead? i.e., allow them to be function_body?*)
type function_body =
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

(*TODO: should be similar to function_body, but also allows calls to carry_ons*)
type carry_on_body = unit

type ty_decl =
  {type_name : type_name;
   type_value : type_value}
let ty_decl_to_string {type_name; type_value} =
  type_name ^ ": " ^ type_value_to_string 2 type_value
type co_decl =
  {decorator : decorator option;
   co_name : function_name;
   co_params : type_value list;
   co_body : carry_on_body}
type fn_decl =
  {fn_name : function_name;
   fn_params : type_value list;
   fn_body : function_body}

(*Top-level declarations. We cannot define types or functions within functions*)
type toplevel_decl =
  | Type of ty_decl
  | Carry_On of co_decl
  | Function of fn_decl
let toplevel_decl_to_string = function
  | Type ty_decl -> "type " ^ ty_decl_to_string ty_decl
  | _ -> failwith "Unsupported"

type program = toplevel_decl list
let program_to_string (p : program) =
  List.map toplevel_decl_to_string p
  |> (fun l -> List.fold_right (fun s acc -> acc ^ s) l "")
