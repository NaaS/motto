(*
   Serialisation-related annotations for types in Crisp
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open General


(*NOTE we allow the annotation to talk about a value that won't be used in
       the program -- but that will be represented in the input (and whose
       value is preserved in the output.)
       By this I mean things like anonymous field names that have type
       annotations -- being anonymous, we cannot read, use, or change their
       value in the program, since we have no way of referring to their value.
       Their value must be preserved however, if, say, we output the (possibly
       otherwise modified) record to the network.

       In order to indicate that a field is anonymous -- i.e., its value is not
       important to the program -- then we use dummy names in type specs
       -- such as in records, in which case the syntax is:
           type bla : record
             a : integer
               { ... }
             _ : integer  # Anonymous field.
               { ... }
             b : integer
               { ... }
*)
type type_annotation_op =
  | Plus
  | Minus
type type_annotation_kind =
  | Ann_Str of string
  | Ann_Int of int
  | Ann_Ident of string
  | Ann_BinaryExp of type_annotation_op * type_annotation_kind * type_annotation_kind

type type_annotation = (string * type_annotation_kind) list

let hadoop_vint_ann_key = "hadoop_vint"
let true_ann_value = Ann_Ident "true"
let is_hadoop_vint =
  List.exists (fun (k, v) -> k = hadoop_vint_ann_key &&
                             v = true_ann_value)

let k_v_string indent (l, e) =
  let rec e_s e = match e with
  | Ann_Str s -> "\"" ^ s ^ "\""
  | Ann_Int i -> string_of_int i
  | Ann_Ident s -> s
  | Ann_BinaryExp (op, e1, e2) ->
    let op_s = match op with
      | Plus -> "+"
      | Minus -> "-"
    in "(" ^ e_s e1 ^ ")" ^ op_s ^ "(" ^ e_s e2 ^ ")"
  in indn indent ^ l ^ " = " ^ e_s e

let ann_string indent indentation ann =
  match ann with
  | [] -> ""
  | k_v :: xs ->
    let indent' = indent + indentation in
    let k_v_string' indent x =
      ",\n"(*FIXME use endline instead of \n?*) ^
      k_v_string indent x
    in
    "\n"(*FIXME check that this agrees with endline*) ^ indn indent' ^
    "{" ^ k_v_string 0 k_v ^
    mk_block(*FIXME instruct mk_block to use endline?*) indent' k_v_string' xs ^ "}"
