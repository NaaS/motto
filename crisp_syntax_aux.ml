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

(*Unwraps a Crisp process body type into a tuple of its components*)
let extract_process_body_bits (ProcessBody (st_decls, e, ex_decls)) =
  (st_decls, e, ex_decls)

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
  | Dictionary (label_opt, idx_ty, ty) ->
    if label_opt = None then
      Dictionary (Some label, idx_ty, ty)
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

(*Determine the type(s) being carried by a container type.*)
let decompose_container (ty : type_value) : type_value list =
  match ty with
  | List (_, ty', _, _) -> [ty']
  | Dictionary (_, _, ty') ->
    (*NOTE we return the value type, not the index type*)
    [ty']
  | Tuple (_, tys') -> tys'
  | Reference (_, ty') -> [ty']
  | RecordType (_, tys', _) -> tys'
  | Disjoint_Union (_, tys') -> tys'
  | _ -> failwith "Type is not a container"

(*Extract the label from a type value.*)
let label_of_type : type_value -> label option = function
  | UserDefinedType (l_opt, _)
  | String (l_opt, _)
  | Integer (l_opt, _)
  | Boolean (l_opt, _)
  | Tuple (l_opt, _)
  | Dictionary (l_opt, _, _)
  | Reference (l_opt, _)
  | Disjoint_Union (l_opt, _) -> l_opt
  | RecordType (l_opt, _, _) -> l_opt
  | List (l_opt, _, _, _) -> l_opt
  | Empty -> failwith "Empty type cannot be given a label"
  | IPv4Address l_opt -> l_opt
  | Undefined -> None
  | ChanType _ -> None

(*Eliminate named parameters, by ordering parameters according to how
  the function expects them to be given.*)
let order_fun_args (fname : function_name) (st : State.state) (args : fun_arg list) : expression list =
  match args with
  | [] -> []
  | Exp _ :: _ ->
    List.map (function
      | Exp e -> e
      | Named _ ->
        failwith "fun_arg values should be either all Exp, or all Named. Expected all Exp.")
         args
  | (Named _) :: _ ->
    let ((chans, arg_tys), ret_tys) =
      List.assoc fname st.State.crisp_funs
      |> extract_function_types in
    let arg_labels =
      List.fold_right (fun ty acc ->
        match label_of_type ty with
        | None -> failwith "Expecting type to be labelled"
        | Some l -> l :: acc) arg_tys []
      |> List.rev in
    assert (chans = []); (*FIXME currently functions cannot be given channel
                           parameters*)
    List.fold_right (fun arg acc ->
      match arg with
      | Named (l, e) ->
        begin
        match General.find_idx arg_labels l with
        | None -> failwith ("Parameter name not found : " ^ l)
        | Some i -> (i, e) :: acc
        end
      | Exp _ -> failwith "fun_arg values should be either all Exp, or all Named. Expected all Named.") args []
    |> List.sort
         (fun (i, _) (j, _) ->
            if i < j then -1 else if i > j then 1 else 0)
    |> List.map snd

(*Lists the constants (and their types) that should be added to the signature.
  Constants are disjunct or field names, in coproducts and labelled products
  respectively.
  NOTE ignores "_", since it's a metasymbol meaning "make this field invisible
       to the program logic, but not to the de/serialiser".*)
let consts_in_type (ty : type_value) : (string * State.identifier_kind * type_value) list option =
  let rec consts_in_type' (ik : State.identifier_kind) = function
    | RecordType (_, tys, _) as ty ->
      List.map (consts_in_type' (State.Field ty)) tys
      |> List.concat
    | Disjoint_Union (_, tys) as ty ->
      List.map (consts_in_type' (State.Disjunct ty)) tys
      |> List.concat
    | ty ->
      match label_of_type ty with
      | None -> failwith "All elements of a type must be named"
      | Some "_" -> []
      | Some label -> [(label, ik, ty)]
  in match ty with
  | RecordType (_, tys, _) ->
    List.map (consts_in_type' (State.Field ty)) tys
    |> List.concat
    |> (fun x -> Some x)
  | Disjoint_Union (_, tys) ->
    List.map (consts_in_type' (State.Disjunct ty)) tys
    |> List.concat
    |> (fun x -> Some x)
  | _ -> None

(*Extract the type of the values that can be received using a channel*)
let rx_chan_type (ct : channel_type) =
  match ct with
  | ChannelSingle (ty, _) -> ty
  | ChannelArray (ty, _, _) -> ty

(*Extract the type of the values that can be sent using a channel*)
let tx_chan_type (ct : channel_type) =
  match ct with
  | ChannelSingle (_, ty) -> ty
  | ChannelArray (_, ty, _) -> ty

(*Indicate whether a type is, or contains, an occurrence of Undefined*)
let rec is_fully_defined_type : type_value -> bool = function
  | UserDefinedType (_, _)
  | String (_, _)
  | Integer (_, _)
  | Boolean (_, _)
  | Empty
  | IPv4Address _ -> true

  | Undefined -> false

  | Tuple (_, tys)
  | RecordType (_, tys, _)
  | Disjoint_Union (_, tys) ->
    List.fold_right (&&) (List.map is_fully_defined_type tys) true

  | List (_, ty, _, _)
  | Reference (_, ty) -> is_fully_defined_type ty

  | Dictionary (_, ty1, ty2) ->
    is_fully_defined_type ty1 && is_fully_defined_type ty2

  | ChanType ct -> is_fully_defined_channel_type ct
and is_fully_defined_channel_type = function
  | ChannelSingle (ty1, ty2) ->
    is_fully_defined_type ty1 && is_fully_defined_type ty2
  | ChannelArray (ty1, ty2, _) ->
    is_fully_defined_type ty1 && is_fully_defined_type ty2

(*Simple form of matching on types.
  ty1 must be ground.
  ty2 can have Undefineds in it.*)
(*NOTE here we require serialisation annotations to be identical*)
let rec type_match (ty1 : type_value) (ty2 : type_value) : bool =
  match ty1, ty2 with
  | (Undefined, _) ->
    failwith "ty1 must be ground"

  | (UserDefinedType (_, _), UserDefinedType (_, _))
  | (String (_, _), String (_, _))
  | (Integer (_, _), Integer (_, _))
  | (Boolean (_, _), Boolean (_, _))
  | (Empty, Empty)
  | (IPv4Address _, IPv4Address _) -> ty1 = ty2

  | (Tuple (_, tys1), Tuple (_, tys2))
  | (RecordType (_, tys1, _), RecordType (_, tys2, _))
  | (Disjoint_Union (_, tys1), Disjoint_Union (_, tys2)) ->
    let ty_pairs = List.combine tys1 tys2 in
    let f (ty1, ty2) = type_match ty1 ty2 in
    List.fold_right (&&) (List.map f ty_pairs) true

  | (List (_, ty1, _, _), List (_, ty2, _, _))
  | (Reference (_, ty1), Reference (_, ty2)) ->
    type_match ty1 ty2

  | (Dictionary (_, ty1A, ty2A), Dictionary (_, ty1B, ty2B)) ->
    type_match ty1A ty1B &&
    type_match ty2A ty2B

  | (ChanType ct1, ChanType ct2) ->
    channel_type_match ct1 ct2
  | (_, Undefined) -> true
  | (_, _) -> false
and channel_type_match (ct1 : channel_type) (ct2 : channel_type) : bool =
  match ct1, ct2 with
  | (ChannelSingle (ty1A, ty2A), ChannelSingle (ty1B, ty2B)) ->
    type_match ty1A ty1B && type_match ty2A ty2B
  | (ChannelArray (ty1A, ty2A, _), ChannelArray (ty1B, ty2B, _)) ->
    type_match ty1A ty1B && type_match ty2A ty2B
  | (_, _) -> false

(*Erases label, making it easier to compare two types.
  NOTE you might need to erase or match type_annotation values too.*)
let rec forget_label (ty : type_value) =
  match ty with
  | UserDefinedType (_, type_name) -> UserDefinedType (None, type_name)
  | String (_, type_annotation) -> String (None, type_annotation)
  | Integer (_, type_annotation) -> Integer (None, type_annotation)
  | Boolean (_, type_annotation) -> Boolean (None, type_annotation)
  | RecordType (_, tys, type_annotation) ->
    (*NOTE we must not erase field names, so we don't recurse on tys*)
    RecordType (None, tys, type_annotation)
  | List (_, ty, dep_idx_opt, type_annotation) ->
    List (None, forget_label ty, dep_idx_opt, type_annotation)
  | Tuple (_, tys) ->
    Tuple (None, List.map forget_label tys)
  | Dictionary (_, idx_ty, ty) ->
    Dictionary (None, forget_label idx_ty, forget_label ty)
  | Reference (_, ty) ->
    Reference (None, forget_label ty)
  | Disjoint_Union (_, _) (*NOTE we must not erase field names, so we don't recurse on tys*)
  | Empty
  | Undefined
  | ChanType _
  | IPv4Address _ -> ty

(*Simple predicate to indicate whether, at the surface level, a type is a
  usertype*)
let is_usertype = function
  | UserDefinedType _ -> true
  | _ -> false

(*Resolves a user-defined type into Flick types.
  if deep_resolution it ensures that the result of the resolution is never
  a user-defined type.
  NOTE that even if using deep_resolution you could have a user-defined type
       but not at the surface level (e.g., as a field of a record).*)
let rec resolve_usertype ?deep_resolution:(deep_resolution : bool = true) (st : State.state) (ty : type_value) : type_value option =
  match ty with
  | UserDefinedType (_, type_name) ->
    begin
    let candidates =
      List.fold_right (fun (ty_n, ty, _) acc ->
        if ty_n = type_name then ty :: acc else acc) st.State.type_declarations [] in
    match candidates with
    | [ty] ->
      if deep_resolution && is_usertype ty then
        resolve_usertype ~deep_resolution st ty
      else Some ty
    | [] -> None
    | _ -> failwith "More than one candidate for resolving a usertype"(*FIXME give more info*)
    end
  | _ -> failwith "Could not resolve usertype"(*FIXME give more info*)

(*If a type is a usertype, then expect to resolve it.
  Otherwise, do nothing.*)
let resolve_if_usertype (st : State.state) (ty : type_value) : type_value =
  match ty with
  | UserDefinedType _ ->
    begin
    match resolve_usertype st ty with
    | None -> failwith "Could not resolve usertype"(*FIXME give more info*)
    | Some ty -> ty
    end
  | _ -> ty

let rec contains_hole : expression -> bool = function
  | Not e
  | Abs e
  | Int_to_address e
  | Address_to_int e
  | LocalDef (_, e)
  | Update (_, e)
  | RecordProjection (e, _)
  | IndexableProjection (_, e)
  | TypeAnnotation (e, _) -> contains_hole e

  | Variable _
  | Int _
  | Str _
  | Meta_quoted _
  | IPv4_address _
  | EmptyList
  | True
  | False -> false

  | And (e1, e2)
  | Or (e1, e2)
  | Equals (e1, e2)
  | GreaterThan (e1, e2)
  | LessThan (e1, e2)
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | ConsList (e1, e2)
  | AppendList (e1, e2)
  | Seq (e1, e2)
  | IntegerRange (e1, e2)
  | Send (e1, e2)
  | Receive (e1, e2)
  | Exchange (e1, e2)
  | UpdateIndexable (_, e1, e2)
  | RecordUpdate (e1, (_, e2))
  | Map (_, e1, e2, _) ->
    contains_hole e1 || contains_hole e2

  | TupleValue es ->
    List.exists contains_hole es

  | ITE (e1, e2, e3_opt) ->
    let b3 =
      match e3_opt with
      | None -> false
      | Some e -> contains_hole e in
    contains_hole e1 || contains_hole e2 || b3

  | Record (l_es) ->
    List.map snd l_es
    |> List.exists contains_hole

  | CaseOf (e, e2s) ->
    contains_hole e ||
      List.exists (fun (e1, e2) ->
        contains_hole e1 || contains_hole e2) e2s

  | Iterate (_, e1, l_e_opt, e2, _) ->
    let b3 =
      match l_e_opt with
      | None -> false
      | Some (_, e) -> contains_hole e in
    contains_hole e1 || contains_hole e2 || b3

  | Functor_App (_, funargs) ->
    List.exists (fun funarg ->
      match funarg with
      | Exp e
      | Named (_, e) -> contains_hole e) funargs

  | Hole -> true

let rec fill_hole (contents : expression) (e : expression) : expression =
  let f = fill_hole contents in
  match e with
  | Hole -> contents
  | Variable _
  | True
  | False
  | Int _
  | IPv4_address _
  | EmptyList
  | Str _
  | Meta_quoted _ -> e

  | TypeAnnotation (e, ty) ->
    TypeAnnotation (f e, ty)

  | Seq (e1, e2) ->
    Seq (f e1, f e2)

  | And (b1, b2) ->
    And (f b1, f b2)
  | Or (b1, b2) ->
    Or (f b1, f b2)
  | Not b' ->
    Not (f b')

  | ITE (be, e1, e2_opt) ->
    let e2_opt' =
      match e2_opt with
      | None -> None
      | Some e -> Some (f e) in
    ITE (f be, f e1, e2_opt')

  | Update (value_name, expression) ->
    Update (value_name, f expression)
  | UpdateIndexable (value_name, idx, expression) ->
    UpdateIndexable (value_name, f idx, f expression)

  | LocalDef ((v, ty_opt), e) ->
    LocalDef ((v, ty_opt), f e)

  | Equals (e1, e2) ->
    Equals (f e1, f e2)

  | GreaterThan (a1, a2) ->
    GreaterThan (f a1, f a2)
  | LessThan (a1, a2) ->
    LessThan (f a1, f a2)

  | Plus (a1, a2) ->
    Plus (f a1, f a2)
  | Minus (a1, a2) ->
    Minus (f a1, f a2)
  | Times (a1, a2) ->
    Times (f a1, f a2)
  | Mod (a1, a2) ->
    Mod (f a1, f a2)
  | Quotient (a1, a2) ->
    Quotient (f a1, f a2)
  | Abs a ->
    Abs (f a)

  | Address_to_int e ->
    Address_to_int (f e)
  | Int_to_address e ->
    Int_to_address (f e)

  | ConsList (x, xs) ->
    ConsList (f x, f xs)
  | AppendList (xs, ys) ->
    AppendList (f xs, f ys)
  | TupleValue xs ->
    TupleValue (List.map f xs)
  | RecordProjection (e, l) ->
    RecordProjection (f e, l)
  | Functor_App (f_name, es) ->
    let es' =
      List.map (function
        | Exp e -> Exp (f e)
        | Named (l, e) -> Named (l, f e)) es in
    Functor_App (f_name, es')
  | Record entries ->
    Record (List.map (fun (l, e) -> (l, f e)) entries)
  | RecordUpdate (r, (l, e)) ->
    RecordUpdate (f r, (l, f e))
  | CaseOf (e, matches) ->
    let matches' =
      List.map (fun (e1, e2) -> (f e1, f e2)) matches in
    CaseOf (f e, matches')
  | IndexableProjection (v, idx) ->
    IndexableProjection (v, f idx)
  | IntegerRange (e1, e2) ->
    IntegerRange (f e1, f e2)
  | Map (v, l, body, unordered) ->
    Map (v, f l, f body, unordered)
  | Iterate (v, l, acc, body, unordered) ->
    let acc' =
      match acc with
      | None -> None
      | Some (l, e) -> Some (l, f e) in
    Iterate (v, f l, acc', f body, unordered)
  | Send (e1, e2) ->
    Send (f e1, f e2)
  | Receive (e1, e2) ->
    Receive (f e1, f e2)
  | Exchange (e1, e2) ->
    Exchange (f e1, f e2)

let funarg_contains_hole : fun_arg -> bool = function
  | Exp e
  | Named (_, e) -> contains_hole e
let funarg_fill_hole (contents : expression) : fun_arg -> fun_arg = function
  | Exp e -> Exp (fill_hole contents e)
  | Named (l, e) -> Named (l, fill_hole contents e)
