(*
   Serialisation-related annotations for types in Crisp
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open General


(*FIXME must allow the annotation to talk about a value that won't be used in
        the program -- but that will be represented in the input (and whose
        value is preserved in the output.)
        Add dummy names in type specs -- such as in records.*)
type type_annotation_kind =
  | Ann_Str of string
  | Ann_Int of int
  | Ann_Ident of string

type type_annotation = (string * type_annotation_kind) list

let hadoop_vint_ann_key = "hadoop_vint"
let true_ann_value = Ann_Ident "true"
let is_hadoop_vint =
  List.exists (fun (k, v) -> k = hadoop_vint_ann_key &&
                             v = true_ann_value)

let k_v_string indent (l, e) =
  let e_s = match e with
  | Ann_Str s -> "\"" ^ s ^ "\""
  | Ann_Int i -> string_of_int i
  | Ann_Ident s -> s in
  indn indent ^ l ^ " = " ^ e_s

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
