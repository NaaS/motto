(*
   Translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Crisp_type_annotation
open Naasty
open Naasty_aux
open State


let require_annotations = false

let unidir_chan_receive_suffix = "_receive_"
let unidir_chan_send_suffix = "_send_"

exception Translation_type of string * type_value
exception Translation_expr of string * expression

let rec naasty_of_flick_type (st : state) (ty : type_value) : (naasty_type * state) =
  let check_and_resolve_typename type_name =
    match lookup_name Type st type_name with
    | None -> failwith ("Undeclared type: " ^ type_name)
    | Some i -> i in
  let check_and_generate_typename typename_opt =
    match typename_opt with
    | None -> failwith "Was expecting type name."
    | Some type_name ->
      begin
        match lookup_name Type st type_name with
        | None -> extend_scope_unsafe Type st type_name
        | Some idx ->
          if forbid_shadowing then
            failwith ("Already declared type: " ^ type_name)
          else
            (idx, st)
      end in
  let check_and_generate_name label_opt =
    match label_opt with
    | None -> (None, st)
    | Some identifier ->
      begin
        match lookup_name Term st identifier with
        | None ->
          let (idx, st') = extend_scope_unsafe Term st identifier
          in (Some idx, st')
        | Some idx ->
          if forbid_shadowing then
            failwith ("Already declared identifier: " ^ identifier)
          else
            (Some idx, st)
      end in
  match ty with
  | Disjoint_Union (_, _) -> failwith "Unsupported"
  | List (_, _, _, _) ->
    (*Lists can be turned into arrays*)
    failwith "Unsupported"
  | Dictionary (label_opt, type_name) ->
    failwith "TODO -- link to dictionary provided by libNaaS" (*TODO*)
  | Empty -> failwith "Cannot translate empty type"
  | Tuple (label_opt, []) ->
    assert (label_opt = None);
    (*We cannot have values of type "void" in the target, we can only type
      functions with such a type.*)
    (Unit_Type, st)
  | Tuple (_, _) ->
    (*Tuples can be turned into records*)
    failwith "Unsupported"
  | UserDefinedType (label_opt, type_name) ->
    let type_name' = check_and_resolve_typename type_name in
    let (label_opt', st') = check_and_generate_name label_opt in
    let ty' = UserDefined_Type (label_opt', type_name')
    in (ty', st')
  | Boolean (label_opt, type_ann) ->
    if (type_ann <> []) then
      failwith "Boolean serialisation annotation not supported"; (*TODO*)
    let (label_opt', st') = check_and_generate_name label_opt in
    let translated_ty = Bool_Type label_opt' in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx translated_ty Term st'
    in (translated_ty, st'')
  | Integer (label_opt, type_ann) ->
    if type_ann = [] && require_annotations then
      raise (Translation_type ("No annotation given", ty))
    else
      let (label_opt', st') = check_and_generate_name label_opt in
      let metadata =
        List.fold_right (fun (name, ann) md ->
          match ann with
          | Ann_Int i ->
            if name = "byte_size" then
              let bits =
                match i with
                | 2 -> 16
                | 4 -> 32
                | 8 -> 64
                | _ -> failwith ("Unsupported integer precision: " ^
                                 string_of_int i ^ " bytes")
              in { md with precision = bits }
            else failwith ("Unrecognised integer annotation: " ^ name)
          | Ann_Ident s ->
            if name = "signed" then
              { md with signed = (bool_of_string s = true) }
            else if name = "hadoop_vint" then
              { md with hadoop_vint = (bool_of_string s = true) }
            else failwith ("Unrecognised integer annotation: " ^ name)
          | _ -> failwith ("Unrecognised integer annotation: " ^ name))
          type_ann default_int_metadata in
    let translated_ty = Int_Type (label_opt', metadata) in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx translated_ty Term st'
    in (translated_ty, st'')
  | IPv4Address label_opt ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let metadata = { signed = false; precision = 32; hadoop_vint = false } in
    let translated_ty = Int_Type (label_opt', metadata) in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx translated_ty Term st'
    in (translated_ty, st'')
  | String (label_opt, type_ann) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let vlen = Undefined (*FIXME determine from type_ann*) in
    let container_type =
      match vlen with
      | Undefined ->
        (*FIXME it's really important to specify stopping conditions, for the
          deserialiser to work -- plus this could also allow us to implement
          bounds checking. This also applies to "Dependent" below.*)
        Reference_Type (label_opt', Char_Type None)
      | Max _ -> Array_Type (label_opt', Char_Type None, vlen)
      | Dependent _ ->
        (*FIXME as in "Undefined" above, we need stopping conditions.*)
        Reference_Type (label_opt', Char_Type None) in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx container_type Term st'
    in (container_type, st'')
  | Reference (label_opt, ty) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let (ty', st'') = naasty_of_flick_type st' ty in
    let translated_ty = Reference_Type (label_opt', ty') in
    let st''' =
      match label_opt' with
      | None -> st''
      | Some idx ->
        update_symbol_type idx translated_ty Term st''
    in (translated_ty, st''')
  | RecordType (label_opt, tys, type_ann) ->
    if (type_ann <> []) then
      failwith "Record serialisation annotation not supported"; (*TODO*)
    let (type_identifier, st') = check_and_generate_typename label_opt in
    let (tys', st'') = fold_map ([], st') naasty_of_flick_type tys in
    let translated_ty = Record_Type (type_identifier, List.rev tys') in
    let st''' =
        update_symbol_type type_identifier translated_ty Type st''
    in (translated_ty, st''')

(* Based on "Translation from Flick to IR" in
   https://github.com/NaaS/system/blob/master/crisp/flick/flick.tex

   Translates a Flick expression into a NaaSty statement -- the first line of
   which will be a declaration of a fresh variable that will hold the
   expression's value, and the last line of which will be an assignment to that
   variable.
   It also returns the updated state -- containing mappings between variable
   indices and names.

   Parameters:
     e: expression to translate
     The other parameters provide context for the translation:
       st: state.
       sts_acc: the program accumulated so far during the translation.
       ctxt_acc: list of indices of fresh variables added to the scope so
         far. list is ordered in the reverse order of the variables' addition to
         the scope. this is used to declare these variables before the code in
         which they will occur; their type information will be obtained from the
         symbol table.
       assign_acc: list of naasty variables to which we should assign the value
         of the current expression. list is ordered in the reverse order to which
         a variable was "subscribed" to the value of this expression.
   Returned: sts_acc sequentially composed with the translated e,
             possibly extended ctxt_acc,
             possibly extended assign_acc,
             possibly extended st
*)
let rec naasty_of_flick_expr (st : state) (e : expression)
          (sts_acc : naasty_statement) (ctxt_acc : identifier list)
          (assign_acc : identifier list) : (naasty_statement *
                                            identifier list (*ctxt_acc*) *
                                            identifier list (*assign_acc*) *
                                            state) =
  let check_and_resolve_name identifier =
    match lookup_name Term st identifier with
    | None -> failwith ("Undeclared identifier: " ^ identifier)
    | Some i -> i in
  match e with
  | Variable value_name ->
    if assign_acc = [] then
      (*We're expecting this variable to be assigned to something -- and that
        something should have been in assign_acc*)
      failwith "assign_acc should not be empty at this point."
    else
      let translated =
        Var (check_and_resolve_name value_name)
        |> lift_assign assign_acc
        |> Naasty_aux.concat
      in (mk_seq sts_acc translated, ctxt_acc,
          [](*Having assigned to assign_accs, we can forget them.*),
          st)
  | Seq (e1, e2) ->
    let (sts_acc', ctxt_acc', assign_acc', st') = naasty_of_flick_expr st e1 sts_acc ctxt_acc assign_acc
    in naasty_of_flick_expr st' e2 sts_acc' ctxt_acc' assign_acc'

  | True
  | False ->
      let translated =
        lift_assign assign_acc (Bool_Value (e = True))
        |> Naasty_aux.concat
      in (mk_seq sts_acc translated, ctxt_acc, assign_acc, st)
  | And (e1, e2)
  | Or (e1, e2)
  | Equals (e1, e2)
  | GreaterThan (e1, e2)
  | LessThan (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Mod (e1, e2)
  | Quotient (e1, e2)
  | Plus (e1, e2) ->
    let ty1, ty2 =
      match e with
      | Crisp_syntax.And (_, _)
      | Crisp_syntax.Or (_, _) ->
        Bool_Type None, Bool_Type None

      | Equals (_, _) (*FIXME for time being we assume that only numbers can be
                              compared for equality*)
      | GreaterThan (_, _)
      | LessThan (_, _)
      | Minus (_, _)
      | Times (_, _)
      | Mod (_, _)
      | Quotient (_, _)
      | Plus (_, _) ->
        let int_ty =
          Int_Type (None, default_int_metadata(*FIXME this should be resolvable
                                                later, when we know more about
                                                where this function will be called
                                                from and what kind of numbers it
                                                uses?*))
        in int_ty, int_ty
      | _ -> failwith "Impossible" in
    let (_, e1_result_idx, st') = mk_fresh Term ~ty_opt:(Some ty1) "x_" 0 st in
    let (sts_acc', ctxt_acc', assign_acc', st'') =
      naasty_of_flick_expr st' e1 sts_acc (e1_result_idx :: ctxt_acc)
        [e1_result_idx] in
    let (_, e2_result_idx, st''') =
      mk_fresh Term ~ty_opt:(Some ty2) "x_" e1_result_idx st'' in
    let (sts_acc'', ctxt_acc'', assign_acc'', st4) =
      naasty_of_flick_expr st''' e2 sts_acc' (e2_result_idx :: ctxt_acc')
        [e2_result_idx] in
    let translated =
      match e with
      | Crisp_syntax.And (_, _) ->
        Naasty.And (Var e1_result_idx, Var e2_result_idx)
      | Crisp_syntax.Or (_, _) ->
        Naasty.Or (Var e1_result_idx, Var e2_result_idx)
      | Equals (_, _) ->
        Naasty.Equals (Var e1_result_idx, Var e2_result_idx)
      | GreaterThan (_, _) ->
        Naasty.GreaterThan (Var e1_result_idx, Var e2_result_idx)
      | LessThan (_, _) ->
        Naasty.LessThan (Var e1_result_idx, Var e2_result_idx)
      | Minus (_, _) ->
        Naasty.Minus (Var e1_result_idx, Var e2_result_idx)
      | Times (_, _) ->
        Naasty.Times (Var e1_result_idx, Var e2_result_idx)
      | Mod (_, _) ->
        Naasty.Mod (Var e1_result_idx, Var e2_result_idx)
      | Quotient (_, _) ->
        Naasty.Quotient (Var e1_result_idx, Var e2_result_idx)
      | Plus (_, _) ->
        Naasty.Plus (Var e1_result_idx, Var e2_result_idx)
      | _ -> failwith "Impossible" in
    let nstmt =
      lift_assign assign_acc translated
      |> Naasty_aux.concat
    in
    assert (assign_acc' = []);
    assert (assign_acc'' = []);
    (mk_seq sts_acc'' nstmt, ctxt_acc'', [], st4)
  | Not e
  | Abs e ->
    let ty =
      match e with
      | Not _ ->
        Bool_Type None
      | Abs _ ->
        Int_Type (None, default_int_metadata(*FIXME this should be resolvable
                                              later, when we know more about
                                              where this function will be called
                                              from and what kind of numbers it
                                              uses?*))
      | _ -> failwith "Impossible" in
    let (_, e_result_idx, st') = mk_fresh Term ~ty_opt:(Some ty) "x_" 0 st in
    let (sts_acc', ctxt_acc', assign_acc', st'') =
      naasty_of_flick_expr st' e sts_acc (e_result_idx :: ctxt_acc)
        [e_result_idx] in
    let translated =
      match e with
      | Not _ ->
        Naasty.Not (Var e_result_idx)
      | Abs _ ->
        Naasty.Abs (Var e_result_idx)
      | _ -> failwith "Impossible" in
    let not_nstmt =
      lift_assign assign_acc translated
      |> Naasty_aux.concat
    in
    assert (assign_acc' = []);
    (mk_seq sts_acc' not_nstmt, ctxt_acc', [], st'')

  | _ -> raise (Translation_expr ("TODO: " ^ expression_to_string 0 e, e))

(*Split a (possibly bidirectional) Crisp channel into a collection of
  unidirectional NaaSty channels.*)
let unidirect_channel (st : state) (Channel (channel_type, channel_name)) : naasty_type list * state =
  let subchan ty direction suffix is_array st =
    let ty', st' = naasty_of_flick_type st ty in
    let _, name_idx, st'' =
      mk_fresh Term ~ty_opt:(Some ty')
        (channel_name ^ suffix) 0(*FIXME const*) st'
    in ([Chan_Type (Some name_idx, is_array, Input, ty')], st'') in
  match channel_type with
  | ChannelSingle (receive_ty, send_ty) ->
    let is_array = false in
    let rec_ty, st' = match receive_ty with
      | Empty -> ([], st)
      | ty ->
        subchan ty Input unidir_chan_receive_suffix is_array st in
    let sen_ty, st'' = match send_ty with
      | Empty -> ([], st')
      | ty ->
        subchan ty Output unidir_chan_send_suffix is_array st'
    in (rec_ty @ sen_ty, st'')
  | ChannelArray (receive_ty, send_ty, dependency) ->
    assert (dependency = None); (*NOTE Dependencies aren't supported*)
    let is_array = true in
    let rec_ty, st' = match receive_ty with
      | Empty -> ([], st)
      | ty ->
        subchan ty Input unidir_chan_receive_suffix is_array st in
    let sen_ty, st'' = match send_ty with
      | Empty -> ([], st')
      | ty ->
        subchan ty Output unidir_chan_send_suffix is_array st'
    in (rec_ty @ sen_ty, st'')

let rec naasty_of_flick_toplevel_decl (st : state) (tl : toplevel_decl) :
  (naasty_declaration * state) =
  match tl with
  | Type ty_decl ->
    let (ty', st') =
      Crisp_syntax_aux.update_empty_label ty_decl.type_name ty_decl.type_value
      |> naasty_of_flick_type st
    in (Type_Decl ty', st')
  | Function fn_decl ->
    (*FIXME might need to prefix function names with namespace, within the
            function body*)
    let ((chans, arg_tys), res_tys) =
      Crisp_syntax_aux.extract_function_types fn_decl.fn_params in
    let (n_arg_tys, st') =
      let standard_types, st' =
        fold_map ([], st) naasty_of_flick_type arg_tys in
      let channel_types, st'' =
        fold_map ([], st') unidirect_channel chans in
      (List.flatten channel_types @ standard_types, st'') in
    let (n_res_ty, st'') =
      match res_tys with
      | [res_ty] -> naasty_of_flick_type st' res_ty
      | _ ->
        (*FIXME restriction*)
        failwith "Currently only functions with single return type are supported." in
    let (_, result_idx, st''') = mk_fresh Term ~ty_opt:(Some n_res_ty) "x_" 0 st'' in
    (*Add type declaration for result_idx, which should be the same as res_ty
      since result_idx will carry the value that's computed in this function.*)
    let init_ctxt = [result_idx] in
    let init_assign_acc = [result_idx] in
    let (init_statmt, ctxt, waiting) = (Skip, init_ctxt, init_assign_acc) in
    let (body, ctxt', waiting', st4) = naasty_of_flick_expr st''' fn_decl.fn_body init_statmt ctxt waiting in

    (*There shouldn't be any more waiting variables at this point, they should
      all have been assigned something.*)
    assert (waiting' = []);

    let body' =
      List.fold_right (fun idx stmt ->
        let ty =
          match lookup_symbol_type idx Term(*all ctxt symbols are term-level*)
                  st4 with
          | None -> failwith ("Couldn't resolve type of ctxt idx " ^
                              string_of_int idx)
          | Some ty -> ty
        in mk_seq (Declaration (ty, None)) stmt) ctxt' body in

    let (fn_idx, st5) =
      if is_fresh fn_decl.fn_name st4 then
        extend_scope_unsafe Term st4 ~ty_opt:None(*FIXME put function's type here?*) fn_decl.fn_name
      else
        failwith ("Function name " ^ fn_decl.fn_name ^ " isn't fresh.")

    in (Fun_Decl (fn_idx, n_arg_tys, n_res_ty,
                (*Add "Return result_idx" to end of function body*)
                  Seq (body', Return (Var result_idx))),
        st5)
  | Process process ->
    (*FIXME!*)(Type_Decl (Bool_Type (Some (-1))), st)
  | Include filename ->
    (*FIXME!*)(Type_Decl (Bool_Type (Some (-1))), st)

let naasty_of_flick_program ?st:((st : state) = initial_state) (p : program) : (naasty_program * state) =
  fold_map ([], st) naasty_of_flick_toplevel_decl p
