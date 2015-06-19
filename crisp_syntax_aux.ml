(*
   Supporting functions for the Crisp syntax definition.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open Crisp_syntax

let name_of_type = function
  | Type ty_decl -> ty_decl.type_name
  | _ -> failwith "Expected type declaration."

(*Unwraps a Crisp function type into a tuple of its components*)
let extract_function_types (FunType (FunDomType (chans, arg_tys), FunRetType ret_tys)) =
  ((chans, arg_tys), ret_tys)

(*Sets the label of a type unless it's already defined. This is used to bridge
  the gap between Flick and NaaSty, since the latter expects all type
  declarations to be associated with their names, while the former distributes
  this a bit (the name is stored in a type record).*)
let update_empty_label label (ty : type_value) =
  match ty with
  | UserDefinedType (label_opt, type_name) ->
    if label_opt = None then
      UserDefinedType (Some label, type_name)
    else failwith "Cannot set an already-set label"
  | String (label_opt, type_annotation) ->
    if label_opt = None then
      String (Some label, type_annotation)
    else failwith "Cannot set an already-set label"
  | Integer (label_opt, type_annotation) ->
    if label_opt = None then
      Integer (Some label, type_annotation)
    else failwith "Cannot set an already-set label"
  | Boolean (label_opt, type_annotation) ->
    if label_opt = None then
      Boolean (Some label, type_annotation)
    else failwith "Cannot set an already-set label"
  | RecordType (label_opt, tys, type_annotation) ->
    if label_opt = None then
      RecordType (Some label, tys, type_annotation)
    else failwith "Cannot set an already-set label"
  | Disjoint_Union (label_opt, tys) ->
    if label_opt = None then
      Disjoint_Union (Some label, tys)
    else failwith "Cannot set an already-set label"
  | List (label_opt, ty, dep_idx_opt, type_annotation) ->
    if label_opt = None then
      List (Some label, ty, dep_idx_opt, type_annotation)
    else failwith "Cannot set an already-set label"
  | Empty -> Empty
  | IPv4Address label_opt ->
    if label_opt = None then
      IPv4Address (Some label)
    else failwith "Cannot set an already-set label"
  | Tuple (label_opt, tys) ->
    if label_opt = None then
      Tuple (Some label, tys)
    else failwith "Cannot set an already-set label"
  | Dictionary (label_opt, ty) ->
    if label_opt = None then
      Dictionary (Some label, ty)
    else failwith "Cannot set an already-set label"
  | Reference (label_opt, ty) ->
    if label_opt = None then
      Reference (Some label, ty)
    else failwith "Cannot set an already-set label"

(*Assuming that the given declaration is a type declaration, this function
  extracts the type being declared.*)
let the_ty_of_decl = function
  | Type ty_decl -> ty_decl.type_value
  | _ -> failwith "Was expecting a type declaration."

let rec ty_of_expr ?strict:(strict : bool = false) (env : (string * type_value) list) : expression -> type_value = function
  | Variable label ->
    begin
      try List.assoc label env
      with Not_found ->
        failwith ("Missing declaration for '" ^ label ^ "'")
    end

  (*Boolean expressions*)
  | True
  | False -> Boolean (None, [])
  | And (e1, e2)
  | Or (e1, e2) ->
    let ans = Boolean (None, []) in
    if strict then
      let f = ty_of_expr ~strict env in
      let (e1_ty, e2_ty) = f e1, f e2 in
      assert (e1_ty = e2_ty);
      assert (e1_ty = ans)
    else ();
    ans
  | Not e ->
    let ans = Boolean (None, []) in
    if strict then
      let e_ty = ty_of_expr ~strict env e in
      assert (e_ty = ans)
    else ();
    ans

  (*Definable over arbitrary types of expressions*)
  | Equals (e1, e2) ->
    let ans = Boolean (None, []) in
    if strict then
      let f = ty_of_expr ~strict env in
      let (e1_ty, e2_ty) = f e1, f e2 in
      assert (e1_ty = e2_ty);
    else ();
    ans

  (*Definable over arithmetic expressions*)
  | GreaterThan (e1, e2)
  | LessThan (e1, e2) ->
    let ans = Boolean (None, []) in
    let expected = Integer (None, []) in
    if strict then
      let f = ty_of_expr ~strict env in
      let (e1_ty, e2_ty) = f e1, f e2 in
      assert (e1_ty = e2_ty);
      assert (e1_ty = expected)
    else ();
    ans

  (*Arithmetic expressions*)
  | Int _ -> Integer (None, [])

  (*NOTE for these expressions we might want to look deeper, to differentiate
    between different kinds of numbers -- ints, floats, etc*)
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2) ->
    let ans = Integer (None, []) in
    if strict then
      let f = ty_of_expr ~strict env in
      let (e1_ty, e2_ty) = f e1, f e2 in
      assert (e1_ty = e2_ty);
      assert (e1_ty = ans)
    else ();
    ans
  | Abs e ->
    let ans = Integer (None, []) in
    if strict then
      let e_ty = ty_of_expr ~strict env e in
      assert (e_ty = ans)
    else ();
    ans

  (*Native representation of an IPv4 address*)
  | IPv4_address (_, _, _, _) -> IPv4Address None
  (*Integer to IP address*)
  | Int_to_address e ->
    let ans = IPv4Address None in
    let expected = Integer (None, []) in
    if strict then
      let e_ty = ty_of_expr ~strict env e in
      assert (e_ty = expected)
    else ();
    ans
  (*IP address to integer*)
  | Address_to_int e ->
    let ans = Integer (None, []) in
    let expected = IPv4Address None in
    if strict then
      let e_ty = ty_of_expr ~strict env e in
      assert (e_ty = expected)
    else ();
    ans

  (*NOTE currently we don't support dependently-typed lists*)
  | _ -> failwith ("TODO")
(*
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
  | Projection of expression * label

  | Function_Call of function_name * fun_arg list

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
*)
