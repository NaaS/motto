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
  | (Exp e) :: rest ->
    List.fold_right (fun arg acc ->
      match arg with
      | Exp e -> e :: acc
      | Named _ -> failwith "fun_arg values should be either all Exp, or all Named. Expected all Exp.") rest [e]
    |> List.rev
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
  respectively.*)
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
      | None -> failwith "Anonymous field -- this may be fine, but check."
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

let rx_chan_type (ct : channel_type) =
  match ct with
  | ChannelSingle (ty, _) -> ty
  | ChannelArray (ty, _, _) -> ty

let tx_chan_type (ct : channel_type) =
  match ct with
  | ChannelSingle (_, ty) -> ty
  | ChannelArray (_, ty, _) -> ty
