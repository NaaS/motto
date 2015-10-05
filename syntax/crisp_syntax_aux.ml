(*
   Supporting functions for the Crisp syntax definition.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open Crisp_syntax

(*Map channel info into type info*)
let chan_to_ty (Channel (cty, cname)) = ChanType (Some cname, cty)

let name_of_type = function
  | Type ty_decl -> ty_decl.type_name
  | _ -> failwith "Expected type declaration."

let name_of_function = function
  | Function {fn_name; _} -> fn_name
  | _ -> failwith "Expected function declaration."

let name_of_decl = function
  | Type {type_name; _} -> type_name
  | Function {fn_name; _} -> fn_name
  | Process {process_name; _} -> process_name
  | d -> failwith ("name_of_decl : cannot extract name from declaration: " ^ toplevel_decl_to_string d)

(*Unwraps a Crisp function type into a tuple of its components*)
let extract_function_types (FunType (dis, FunDomType (chans, arg_tys), FunRetType ret_tys)) =
  (dis, (chans, arg_tys), ret_tys)

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
  | Undefined s -> Some s
  | ChanType (l_opt, _) -> l_opt

let label_of_channel (Channel (_, channel_name)) = channel_name

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
    let (_(*ignore dependent parameters*), (chans, arg_tys), ret_tys) =
      List.assoc fname st.State.crisp_funs
      |> snd (*it shouldn't matter if we're dealing with a call to a function or
              a process*)
      |> extract_function_types in
    let chan_labels =
        List.map label_of_channel chans in
    let arg_labels =
      List.fold_right (fun ty acc ->
        match label_of_type ty with
        | None -> failwith "Expecting type to be labelled"
        | Some l -> l :: acc) arg_tys []
      |> List.rev in
    let complete_arg_list = chan_labels @ arg_labels in
    List.fold_right (fun arg acc ->
      match arg with
      | Named (l, e) ->
        begin
        match General.find_idx complete_arg_list l with
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

  | Undefined _ -> false

  | Tuple (_, tys)
  | RecordType (_, tys, _)
  | Disjoint_Union (_, tys) ->
    List.fold_right (&&) (List.map is_fully_defined_type tys) true

  | List (_, ty, _, _)
  | Reference (_, ty) -> is_fully_defined_type ty

  | Dictionary (_, ty1, ty2) ->
    is_fully_defined_type ty1 && is_fully_defined_type ty2

  | ChanType (_, ct) -> is_fully_defined_channel_type ct
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
  | (Undefined _, _) ->
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

  | (ChanType (_, ct1), ChanType (_, ct2)) ->
    channel_type_match ct1 ct2
  | (_, Undefined _(*NOTE i ignore variable name*)) -> true
  | (_, _) -> false
and channel_type_match (ct1 : channel_type) (ct2 : channel_type) : bool =
  match ct1, ct2 with
  | (ChannelSingle (ty1A, ty2A), ChannelSingle (ty1B, ty2B)) ->
    type_match ty1A ty1B && type_match ty2A ty2B
  | (ChannelArray (ty1A, ty2A, _), ChannelArray (ty1B, ty2B, _)) ->
    type_match ty1A ty1B && type_match ty2A ty2B
  | (_, _) -> false

(*Simple form of unification on types.
  (We only have a single unification variable, encoded using "Undefined"
  -- i ignore the parameter).*)
(*NOTE here we ignore serialisation annotations and names*)
let rec type_unify (ty1 : type_value) (ty2 : type_value) : type_value option =
  let unify_pairs (tys1 : type_value list) (tys2 : type_value list) : type_value list option =
    let ty_pairs = List.combine tys1 tys2 in
    let f (ty1, ty2) = type_unify ty1 ty2 in
    List.fold_right (fun result acc ->
      match acc with
      | None -> None
      | Some acc ->
        begin
          match result with
          | None -> None
          | Some ty' -> Some (ty' :: acc)
        end) (List.map f ty_pairs) (Some []) in
  match ty1, ty2 with
  | (Undefined _, Undefined _) -> Some ty1
  | (Undefined _, _) -> Some ty2
  | (_, Undefined _) -> Some ty1

  | (UserDefinedType (_, tyname1), UserDefinedType (_, tyname2)) ->
    if tyname1 = tyname2 then Some ty1
    else None

  | (String (_, _), String (_, _))
  | (Integer (_, _), Integer (_, _))
  | (Boolean (_, _), Boolean (_, _))
  | (Empty, Empty)
  | (IPv4Address _, IPv4Address _) -> Some ty1

  | (Tuple (_, tys1), Tuple (_, tys2)) ->
    begin
      match unify_pairs tys1 tys2 with
      | None -> None
      | Some tys' -> Some (Tuple (None, tys'))
    end
  | (RecordType (_, tys1, _), RecordType (_, tys2, _)) ->
    begin
      match unify_pairs tys1 tys2 with
      | None -> None
      | Some tys' -> Some (RecordType (None, tys', []))
    end
  | (Disjoint_Union (_, tys1), Disjoint_Union (_, tys2)) ->
    begin
      match unify_pairs tys1 tys2 with
      | None -> None
      | Some tys' -> Some (Disjoint_Union (None, tys'))
    end

  | (List (_, ty1, di1, _), List (_, ty2, di2, _)) ->
    if di1 <> di2 then None
    else
      begin
        match type_unify ty1 ty2 with
        | None -> None
        | Some ty' -> Some (List (None, ty', di1, []))
      end
  | (Reference (_, ty1), Reference (_, ty2)) ->
    begin
      match type_unify ty1 ty2 with
      | None -> None
      | Some ty' -> Some (Reference (None, ty'))
    end

  | (Dictionary (_, ty1A, ty2A), Dictionary (_, ty1B, ty2B)) ->
    begin
      match type_unify ty1A ty1B, type_unify ty2A ty2B with
      | None, _ -> None
      | _, None -> None
      | Some ty1, Some ty2 -> Some (Dictionary (None, ty1, ty2))
    end

  | (ChanType (_, ct1), ChanType (_, ct2)) ->
    match channel_type_unify ct1 ct2 with
    | None -> None
    | Some ct -> Some (ChanType (None, ct))
and channel_type_unify (ct1 : channel_type) (ct2 : channel_type) : channel_type option =
  match ct1, ct2 with
  | (ChannelSingle (ty1A, ty2A), ChannelSingle (ty1B, ty2B)) ->
    begin
      match type_unify ty1A ty1B, type_unify ty2A ty2B with
      | None, _ -> None
      | _, None -> None
      | Some ty1, Some ty2 -> Some (ChannelSingle (ty1, ty2))
    end
  | (ChannelArray (ty1A, ty2A, di1), ChannelArray (ty1B, ty2B, di2)) ->
    if di1 <> di2 then None
    else
      begin
        match type_unify ty1A ty1B, type_unify ty2A ty2B with
        | None, _ -> None
        | _, None -> None
        | Some ty1, Some ty2 -> Some (ChannelArray (ty1, ty2, di1))
      end

(*Given a pair of types, extract all pairings between an occurrence of Undefined
  in one type and the corresponding type in the other
  This is used since we don't have a full implementation of first-order
  unification, and this serves to compute the unifier to the single unification
  variable we have (i.e., "Undefined").*)
(*NOTE here we ignore serialisation annotations and names*)
let rec extract_unifier (ty1 : type_value) (ty2 : type_value) : type_value list =
  let extract_unifiers (tys1 : type_value list) (tys2 : type_value list) : type_value list =
    let ty_pairs = List.combine tys1 tys2 in
    let f (ty1, ty2) = extract_unifier ty1 ty2 in
    List.map f ty_pairs
    |> List.flatten in
  match ty1, ty2 with
  | (Undefined _, _) -> [ty2]
  | (_, Undefined _) -> [ty1]

  | (UserDefinedType (_, _), UserDefinedType (_, _))
  | (String (_, _), String (_, _))
  | (Integer (_, _), Integer (_, _))
  | (Boolean (_, _), Boolean (_, _))
  | (Empty, Empty)
  | (IPv4Address _, IPv4Address _) -> []

  | (Tuple (_, tys1), Tuple (_, tys2))
  | (RecordType (_, tys1, _), RecordType (_, tys2, _))
  | (Disjoint_Union (_, tys1), Disjoint_Union (_, tys2)) -> extract_unifiers tys1 tys2

  | (List (_, ty1, _, _), List (_, ty2, _, _)) -> extract_unifier ty1 ty2

  | (Dictionary (_, ty1A, ty2A), Dictionary (_, ty1B, ty2B)) ->
    extract_unifier ty1A ty1B @ extract_unifier ty2A ty2B

  | (ChanType (_, ct1), ChanType (_, ct2)) -> channel_extract_unifier ct1 ct2
and channel_extract_unifier (ct1 : channel_type) (ct2 : channel_type) : type_value list =
  match ct1, ct2 with
  | (ChannelSingle (ty1A, ty2A), ChannelSingle (ty1B, ty2B)) ->
    extract_unifier ty1A ty1B @ extract_unifier ty2A ty2B
  | (ChannelArray (ty1A, ty2A, di1), ChannelArray (ty1B, ty2B, di2)) ->
    extract_unifier ty1A ty1B @ extract_unifier ty2A ty2B

let unique_unifier (unifiers : type_value list) : type_value option =
  match unifiers with
  | [] -> None
  | (u :: us) ->
    let unifier =
      List.fold_right (fun u acc ->
        if u <> acc then failwith "Unifiers should be identical"(*FIXME give more info*)
        else acc) us u
    in Some unifier

(*NOTE we ignore the name of the unification variable*)
let rec apply_unifier (unifier : type_value) (ty : type_value) : type_value =
  assert (not (undefined_ty unifier));
  match ty with
  | Undefined _ -> unifier
  | UserDefinedType (_, _)
  | String (_, _)
  | Integer (_, _)
  | Boolean (_, _)
  | Empty
  | IPv4Address _ -> ty
  | RecordType (label_opt, tys, ty_ann) ->
    RecordType (label_opt, List.map (apply_unifier unifier) tys, ty_ann)
  | Disjoint_Union (label_opt, tys) ->
    Disjoint_Union (label_opt, List.map (apply_unifier unifier) tys)
  | List (label_opt, ty, di_opt, ty_ann) ->
    List (label_opt, apply_unifier unifier ty, di_opt, ty_ann)
  | Tuple (label_opt, tys) ->
    Tuple (label_opt, List.map (apply_unifier unifier) tys)
  | Dictionary (label_opt, ty1, ty2) ->
    Dictionary (label_opt, apply_unifier unifier ty1, apply_unifier unifier ty2)
  | Reference (label_opt, ty) -> Reference (label_opt, apply_unifier unifier ty)
  | ChanType (label_opt, ct) ->
    ChanType (label_opt, channel_type_apply_unifier unifier ct)
and channel_type_apply_unifier unifier = function
  | ChannelSingle (ty1, ty2) ->
    ChannelSingle (apply_unifier unifier ty1, apply_unifier unifier ty2)
  | ChannelArray (ty1, ty2, di_opt) ->
    ChannelArray (apply_unifier unifier ty1, apply_unifier unifier ty2, di_opt)

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
(*  | Exchange (e1, e2)*)
  | UpdateIndexable (_, e1, e2)
  | RecordUpdate (e1, (_, e2))
  | Map (_, e1, e2, _) ->
    contains_hole e1 || contains_hole e2

  | Send (_, (_, idx_opt), e) ->
    let b =
      match idx_opt with
      | None -> false
      | Some idx -> contains_hole idx in
    b || contains_hole e
  | Receive (_, (_, idx_opt))
  | Peek (_, (_, idx_opt)) ->
    begin
    match idx_opt with
    | None -> false
    | Some idx -> contains_hole idx
    end

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
  | Send (inv, (c_name, idx_opt), e) ->
    Send (inv, (c_name, General.bind_opt (fun x -> Some (f x)) None idx_opt), f e)
  | Receive (inv, (c_name, idx_opt)) ->
    Receive (inv, (c_name, General.bind_opt (fun x -> Some (f x)) None idx_opt))
  | Peek (inv, (c_name, idx_opt)) ->
    Peek (inv, (c_name, General.bind_opt (fun x -> Some (f x)) None idx_opt))
(*
  | Exchange (e1, e2) ->
    Exchange (f e1, f e2)
*)

let funarg_contains_hole : fun_arg -> bool = function
  | Exp e
  | Named (_, e) -> contains_hole e
let funarg_fill_hole (contents : expression) : fun_arg -> fun_arg = function
  | Exp e -> Exp (fill_hole contents e)
  | Named (l, e) -> Named (l, fill_hole contents e)

(*Map an expression list into a Flick list*)
let flick_list (l : expression list) : expression =
  List.fold_right (fun x l -> ConsList (x, l)) (List.rev l) EmptyList

(*Convert OCaml integer list into a Flick integer list*)
let flick_integer_list (l : int list) : expression =
  List.map (fun i -> Int i) l
  |> flick_list

let rec subst_var (v : string) (u : expression) (e : expression) : expression =
  match e with
  | Variable l -> if l = v then u else e
  | TypeAnnotation (e, ty) -> TypeAnnotation (subst_var v u e, ty)

  | True
  | False
  | Int _
  | IPv4_address _
  | EmptyList
  | Str _
  | Meta_quoted _
  | Hole -> e

  | And (e1, e2) -> And (subst_var v u e1, subst_var v u e2)
  | Or (e1, e2) -> Or (subst_var v u e1, subst_var v u e2)
  | Equals (e1, e2) -> Equals (subst_var v u e1, subst_var v u e2)
  | GreaterThan (e1, e2) -> GreaterThan (subst_var v u e1, subst_var v u e2)
  | LessThan (e1, e2) -> LessThan (subst_var v u e1, subst_var v u e2)
  | Plus (e1, e2) -> Plus (subst_var v u e1, subst_var v u e2)
  | Minus (e1, e2) -> Minus (subst_var v u e1, subst_var v u e2)
  | Times (e1, e2) -> Times (subst_var v u e1, subst_var v u e2)
  | Mod (e1, e2) -> Mod (subst_var v u e1, subst_var v u e2)
  | Quotient (e1, e2) -> Quotient (subst_var v u e1, subst_var v u e2)
  | ConsList (e1, e2) -> ConsList (subst_var v u e1, subst_var v u e2)
  | AppendList (e1, e2) -> AppendList (subst_var v u e1, subst_var v u e2)
  | Seq (e1, e2) -> Seq (subst_var v u e1, subst_var v u e2)

  | Send (inv, (c_name, idx_opt), e) ->
    let c_name', inv' =
      if v = c_name then
        match u with
        | Variable x -> x, inv
        | InvertedVariable x -> x, not inv
        | _ -> failwith "Channel name cannot be an arbitrary expression"
      else c_name, inv in
    Send (inv', (c_name',
           General.bind_opt (fun idx -> Some (subst_var v u idx)) None idx_opt),
          subst_var v u e)
  | Receive (inv, (c_name, idx_opt)) ->
    let c_name', inv' =
      if v = c_name then
        match u with
        | Variable x -> x, inv
        | InvertedVariable x -> x, not inv
        | _ -> failwith "Channel name cannot be an arbitrary expression"
      else c_name, inv in
    Receive (inv',
             (c_name', General.bind_opt (fun idx ->
                Some (subst_var v u idx)) None idx_opt))
  | Peek (inv, (c_name, idx_opt)) ->
    let c_name', inv' =
      if v = c_name then
        match u with
        | Variable x -> x, inv
        | InvertedVariable x -> x, not inv
        | _ -> failwith "Channel name cannot be an arbitrary expression"
      else c_name, inv in
    Peek (inv',
             (c_name', General.bind_opt (fun idx ->
                Some (subst_var v u idx)) None idx_opt))

  | Not e' -> Not (subst_var v u e')
  | Abs e' -> Abs (subst_var v u e')
  | Int_to_address e' -> Int_to_address (subst_var v u e')
  | Address_to_int e' -> Address_to_int (subst_var v u e')

  | TupleValue es -> TupleValue (List.map (subst_var v u) es)

  | ITE (e1, e2, e3_opt) ->
    let e3_opt' =
      match e3_opt with
      | None -> None
      | Some e3 -> Some (subst_var v u e3) in
    ITE (subst_var v u e1, subst_var v u e2, e3_opt')
  | LocalDef (ty, e) -> LocalDef (ty, subst_var v u e)
  | Update (value_name, e) -> Update (value_name, subst_var v u e)
  | UpdateIndexable (value_name, e1, e2) ->
    UpdateIndexable (value_name, subst_var v u e1, subst_var v u e2)

  | RecordProjection (e, l) -> RecordProjection (subst_var v u e, l)

  | Functor_App (f_name, args) ->
    let args' =  List.map (function
      | Exp e -> Exp (subst_var v u e)
      | Named (l, e) -> Named (l, subst_var v u e)) args in
    Functor_App (f_name, args')

  | Record fields ->
    let fields' = List.map (fun (l, e) -> (l, subst_var v u e)) fields in
    Record fields'
  | RecordUpdate (e1, (l, e2)) ->
    RecordUpdate (subst_var v u e1, (l, subst_var v u e2))

  | CaseOf (e, cases) ->
    let cases' = List.map (fun (e1, e2) -> (subst_var v u e1, subst_var v u e2)) cases in
    CaseOf (subst_var v u e, cases')

  | IndexableProjection (l, e) -> IndexableProjection (l, subst_var v u e)

  | IntegerRange (e1, e2) -> IntegerRange (subst_var v u e1, subst_var v u e2)
  | Map (l, e1, e2, b) -> Map (l, subst_var v u e1, subst_var v u e2, b)
  | Iterate (l, e, acc_opt, body_e, b) ->
    let acc_opt' =
      match acc_opt with
      | None -> None
      | Some (l, acc_e) -> Some (l, subst_var v u acc_e) in
    Iterate (l, subst_var v u e, acc_opt', subst_var v u body_e, b)

(*Return the list of variables that are bound (via LocalDef) in a function*)
let rec bound_vars (e : expression) (acc : label list) : label list =
  match e with
  | LocalDef ((v, _), e) ->
    bound_vars e (v :: acc)

  | Variable _
  | InvertedVariable _
  | True
  | False
  | Int _
  | IPv4_address _
  | EmptyList
  | Str _
  | Meta_quoted _
  | Hole -> acc
  | TypeAnnotation (e, _) -> bound_vars e acc

  | Not e
  | Abs e
  | Int_to_address e
  | Address_to_int e
  | Update (_, e)
  | RecordProjection (e, _)
  | IndexableProjection (_, e) -> bound_vars e acc

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
  | UpdateIndexable (_, e1, e2)
  | IntegerRange (e1, e2)
  | RecordUpdate (e1, (_, e2))
  | Map (_, e1, e2, _) ->
    bound_vars e1 acc
    |> bound_vars e2

  | TupleValue es ->
    List.fold_right bound_vars es acc
  | Record labelled_es ->
    List.fold_right (fun (_, e) -> bound_vars e) labelled_es acc
  | CaseOf (e', e_pairs) ->
    List.fold_right (fun (e1, e2) acc ->
      bound_vars e1 acc
      |> bound_vars e2) e_pairs (bound_vars e' acc)

  | ITE (e1, e2, e3_opt) ->
    let acc' =
      bound_vars e1 acc
      |> bound_vars e2 in
    General.bind_opt (fun e -> bound_vars e acc') acc' e3_opt

  | Iterate (_, e1, labelled_e_opt, e2, _) ->
    let acc' =
      bound_vars e1 acc
      |> bound_vars e2 in
    General.bind_opt (fun (_, e) -> bound_vars e acc') acc' labelled_e_opt

  | Receive (_, (_, idx_opt))
  | Peek (_, (_, idx_opt)) ->
    General.bind_opt (fun e -> bound_vars e acc) acc idx_opt
  | Send (_, (_, idx_opt), e') ->
    General.bind_opt (fun e -> bound_vars e acc) acc idx_opt
    |> bound_vars e'

  | Functor_App (_, fun_args) ->
    List.fold_right (fun funarg acc ->
      match funarg with
      | Exp e
      | Named (_, e) ->
        bound_vars e acc) fun_args acc
