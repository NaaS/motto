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
        Pointer_Type (label_opt', Char_Type None)
      | Max _ -> Array_Type (label_opt', Char_Type None, vlen)
      | Dependent _ ->
        (*FIXME as in "Undefined" above, we need stopping conditions.*)
        Pointer_Type (label_opt', Char_Type None) in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx container_type Term st'
    in (container_type, st'')
  | Reference (label_opt, ty) ->
    let (label_opt', st') = check_and_generate_name label_opt in
    let (ty', st'') = naasty_of_flick_type st' ty in
    let translated_ty = Pointer_Type (label_opt', ty') in
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

(*If the name exists in the name mapping, then map it, otherwise leave the name
  as it is*)
let try_local_name (l : label)
      (local_name_map : (label * label) list) : label =
  try List.assoc l local_name_map
  with Not_found -> l
(*Add a local name: a mapping from a programmer-chosen name (within this scope)
  to the name that the compiler decided to use for this. The latter may be
  based on the former, but could be different in order to deal with shadowing.*)
let extend_local_names (local_name_map : (label * label) list)
      (name : label) (name' : identifier) (st : state) : (label * label) list =
  (name, resolve_idx Term no_prefix (Some st) name') :: local_name_map

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
       local_name_map: see comment to function extend_local_names.
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
          (local_name_map : (label * label) list)
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
      (*If assign_acc is empty it means that we've got an occurrence of a
        variable that doesn't contribute to an expression. For instance, you
        could have Seq (Variable _, _). In this case we emit a warning, to alert
        the programmer of the dead code, and emit a comment in NaaSty.*)
      let _  = prerr_endline ("warning: unused variable " ^ value_name)
      (*FIXME would be nice to be able to report line & column numbers*) in
      let translated =
        Commented (Skip, "Evaluated variable " ^ expression_to_string no_indent e)
      in (mk_seq sts_acc translated, ctxt_acc,
          [](*since assign_acc=[] to start with*),
          st)
    else
      let translated =
        try_local_name value_name local_name_map
        |> check_and_resolve_name
        |> (fun x -> Var x) (*whither eta?*)
        |> lift_assign assign_acc
        |> Naasty_aux.concat
      in (mk_seq sts_acc translated, ctxt_acc,
          [](*Having assigned to assign_accs, we can forget them.*),
          st)
  | Seq (e1, e2) ->
    let (sts_acc', ctxt_acc', assign_acc', st') =
      (*We given the evaluation of e1 an empty assign_acc, since the assign_acc
        we received is intended for e2.*)
      naasty_of_flick_expr st e1 local_name_map sts_acc ctxt_acc []
    in
      assert (assign_acc' = []); (*We shouldn't be getting anything to assign
                                   from e1, particularly as we gave it an empty
                                   assign_acc*)
      naasty_of_flick_expr st' e2 local_name_map sts_acc' ctxt_acc' assign_acc

  | True
  | False ->
      let translated =
        lift_assign assign_acc (Bool_Value (e = True))
        |> Naasty_aux.concat
      in
      (*having assigned to assign_acc, we've done our duty, so we return an
        empty assign_acc*)
      (mk_seq sts_acc translated, ctxt_acc, [], st)
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
      naasty_of_flick_expr st' e1 local_name_map sts_acc (e1_result_idx :: ctxt_acc)
        [e1_result_idx] in
    let (_, e2_result_idx, st''') =
      mk_fresh Term ~ty_opt:(Some ty2) "x_" e1_result_idx st'' in
    let (sts_acc'', ctxt_acc'', assign_acc'', st4) =
      naasty_of_flick_expr st''' e2 local_name_map sts_acc' (e2_result_idx :: ctxt_acc')
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
        Naasty.Gt (Var e1_result_idx, Var e2_result_idx)
      | LessThan (_, _) ->
        Naasty.Lt (Var e1_result_idx, Var e2_result_idx)
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
      (*NOTE this means that if assign_acc=[] then we'll never see `translated`
             appear in the intermediate program.*)
      |> Naasty_aux.concat
    in
    assert (assign_acc' = []);
    assert (assign_acc'' = []);
    (mk_seq sts_acc'' nstmt, ctxt_acc'', [], st4)
  | Not e'
  | Abs e' ->
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
      naasty_of_flick_expr st' e' local_name_map sts_acc (e_result_idx :: ctxt_acc)
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

  | Int i ->
    let translated = Int_Value i in
    let nstmt =
      lift_assign assign_acc translated
      |> Naasty_aux.concat
    in
    (mk_seq sts_acc nstmt, ctxt_acc, [], st)

  | Iterate (label, range_e, acc_opt, body_e, unordered) ->
    assert (not unordered); (*FIXME currently only supporting ordered iteration*)

    (*NOTE Since the translation of this form of expression is more complex
      than previous ones, it's worth re-capping the role of the contextual
      variables:
        assign_acc: this contains what we need to assign to at the end of the
          translation. You'll notice that there are lots of asserts related to
          its value: we use this to ensure that recursive calls to
          naasty_of_flick_expr don't leave us with things _we_ (i.e., the
          current invocation) need to assign to on their behalf. In calls to
          naasty_of_flick_expr we give this the list of variables that we expect
          will be assigned the value of the translated expression.

        sts_acc grows to accumulate the translated program.

        ctxt_acc grows to accumulate the context of the translated program --
          the context in this case consists of variable declarations that need
          to precede the program.
    *)

    (*If we have an acc_opt value, then declare and assign to such a value*)
    let ((sts_acc', ctxt_acc', assign_acc', st'), local_name_map',
         acc_label_idx_opt) =
      match acc_opt with
      | None -> ((sts_acc, ctxt_acc, assign_acc, st), local_name_map, None)
      | Some (acc_label, acc_init) ->
        let acc_init_ty = (*FIXME use type inference*)
          Int_Type (None, default_int_metadata) in
        (*Since this is a newly-declared variable, make sure it's fresh.*)
        let (_, acc_label_idx, st') =
          mk_fresh Term ~ty_opt:(Some acc_init_ty) (acc_label ^ "_") 0 st in
        let translate_initialisation =
          naasty_of_flick_expr st' acc_init local_name_map sts_acc
            (acc_label_idx :: ctxt_acc) [acc_label_idx] in
        (translate_initialisation,
         extend_local_names local_name_map acc_label acc_label_idx st',
         Some acc_label_idx) in
    assert (assign_acc' = []);

    (*FIXME move this somewhere more general?*)
    let int_ty = Int_Type (None, default_int_metadata) in

    (*Determine the "from" and "until" values of the loop.
      NOTE we ignore the value we'd get for assign_acc'', since this should be
           [] -- we ensure that this is so, and assert that intermediate values
           of this are [] too, as can be seen below.*)
    let (sts_acc'', ctxt_acc'', _, st'', from_idx, to_idx) =
      match range_e with
      | IntegerRange (from_e, until_e)->
        let (_, from_idx, st'') =
          mk_fresh Term ~ty_opt:(Some int_ty) "from_" 0 st' in
        let (sts_acc'', ctxt_acc'', assign_acc'', st''') =
          naasty_of_flick_expr st'' from_e local_name_map sts_acc'
            (from_idx :: ctxt_acc') [from_idx] in
        assert (assign_acc'' = []);

        let (_, to_idx, st4) =
          mk_fresh Term ~ty_opt:(Some int_ty) "to_" 0 st''' in
        let (sts_acc''', ctxt_acc''', assign_acc''', st5) =
          naasty_of_flick_expr st4 until_e local_name_map sts_acc''
            (to_idx :: ctxt_acc'') [to_idx] in
        assert (assign_acc''' = []);

        (sts_acc''', ctxt_acc''', [], st5, from_idx, to_idx)
      | _ -> failwith "Unsupported iteration range" in

    let (_, idx, st''') =
      mk_fresh Term ~ty_opt:(Some int_ty) (label ^ "_") 0 st'' in
    let idx_ty = the (lookup_symbol_type idx Term st''') in
    let (_, body_result_idx, st''') =
      mk_fresh Term
        ~ty_opt:(Some int_ty)(*FIXME this should be same as type of acc, since
                                     after all we'll be assigning this to acc*)
        ("body_result_") 0 st''' in

    let local_name_map'' =
      extend_local_names local_name_map' label idx st''' in

    let condition = LEq (Var from_idx, Var to_idx) in
    let increment = Increment (idx, Int_Value 1) in

    (*The tail statement is added to the body, it's the last statement executed
      in the body. It serves to update the acc, if one is being used.*)
    let tail_statement =
      match acc_label_idx_opt with
      | None -> Skip
      | Some acc_label_idx ->
        Assign (Var acc_label_idx, Var body_result_idx) in

    let (body, ctxt_acc''', assign_acc''', st4) =
      naasty_of_flick_expr st''' body_e local_name_map''
        Skip (*At the start of the translation, the body is completely empty,
               thus Skip.*)
        (body_result_idx :: ctxt_acc'') [body_result_idx] in
    assert (assign_acc''' = []);

    (*As always, we assign to any waiting variables. In the case of loops, we
      can only do this if an acc has been defined.*)
    let nstmt =
      match acc_label_idx_opt with
      | None -> Skip
      | Some acc_label_idx ->
          lift_assign assign_acc (Var acc_label_idx)
          |> Naasty_aux.concat in

    let for_stmt =
      For ((idx_ty, condition, increment), mk_seq body tail_statement) in
    (Naasty_aux.concat [sts_acc''; for_stmt; nstmt], ctxt_acc''', [], st4)

  | Crisp_syntax.ITE (be, e1, e2_opt) -> 
     (*FIXME: if are expressions, so they should assume the value of either the then or the else branch, so 
      we should allocate a variable for that and assign the expression of one of the two to that variable *)
    let (_, cond_result_idx, st_before_cond) = mk_fresh Term ~ty_opt:(Some (Bool_Type None) (*Fixme: not sure about the None *)) "ifcond_" 0 st in
    let (sts_acc_cond, ctxt_acc_cond, assign_acc_cond, st_cond) =
      naasty_of_flick_expr st_before_cond be local_name_map sts_acc (cond_result_idx :: ctxt_acc) [cond_result_idx] in
    assert (assign_acc_cond = []); (* should this be empty? not sure *)
    let (_, if_result_idx, st_before_then) = mk_fresh Term ~ty_opt:(Some (Int_Type (None, default_int_metadata))) (*Fixme: not sure about type *) "ifresult_" 0 st_cond in
    let (then_block, ctxt_acc_then, assign_acc_then, st_then) = 
      naasty_of_flick_expr st_before_then e1 local_name_map Skip (if_result_idx :: ctxt_acc_cond) [if_result_idx] in
    let (else_block, ctxt_acc_else, assign_acc_else, st_else) = 
      naasty_of_flick_expr st_then (the e2_opt) local_name_map Skip ctxt_acc_then [if_result_idx] in
    let translated = 
      Naasty.If ( Var cond_result_idx, then_block, else_block) in
    let nstmt =
      lift_assign assign_acc (Var if_result_idx) in
    (Naasty_aux.concat (List.concat [[sts_acc_cond; translated]; nstmt]), ctxt_acc_else, assign_acc_else, st_else);
      
  | _ -> raise (Translation_expr ("TODO: " ^ expression_to_string no_indent e, e))

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

(*Turns a Flick function body into a NaaSty one. The content of processes could
  also be regarded to be function bodies.*)
let naasty_of_flick_function_body (ctxt : Naasty.identifier list)
      (waiting : Naasty.identifier list) (init_statmt : naasty_statement)
      (flick_body : Crisp_syntax.expression) (st : state) =
  let (body, ctxt', waiting', st') =
    naasty_of_flick_expr st flick_body [](*local_name_map is local to function
                                           blocks, so we start out with an empty
                                           map when translating a function block.*)
      init_statmt ctxt waiting in
  (*There shouldn't be any more waiting variables at this point, they should
    all have been assigned something.*)
  assert (waiting' = []);
  let body' =
    List.fold_right (fun idx stmt ->
      let ty =
        match lookup_symbol_type idx Term(*all ctxt symbols are term-level*)
                st' with
        | None -> failwith ("Couldn't resolve type of ctxt idx " ^
                            string_of_int idx)
        | Some ty -> ty
      in mk_seq (Declaration (ty, None)) stmt) ctxt' body
  in (body', st')

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
      | [] -> (Unit_Type, st')
      | [res_ty] -> naasty_of_flick_type st' res_ty
      | _ ->
        (*FIXME restriction*)
        failwith "Currently only functions with single return type are supported." in

    let init_statmt = Skip in
    let body'', st4 =
      if n_res_ty = Unit_Type then
        let init_ctxt = [] in
        let init_assign_acc = [] in
        let body', st4 =
          naasty_of_flick_function_body init_ctxt init_assign_acc init_statmt
            fn_decl.fn_body st'' in
        let body'' =
          (*Add "Return" to end of function body*)
          Seq (body', Return None)
        in (body'', st4)
      else
        let (_, result_idx, st''') = mk_fresh Term ~ty_opt:(Some n_res_ty) "x_" 0 st'' in
        (*Add type declaration for result_idx, which should be the same as res_ty
          since result_idx will carry the value that's computed in this function.*)
        let init_ctxt = [result_idx] in
        let init_assign_acc = [result_idx] in
        let body', st4 =
          naasty_of_flick_function_body init_ctxt init_assign_acc init_statmt
            fn_decl.fn_body st''' in
        let body'' =
          assert (n_res_ty <> Unit_Type); (*Unit functions should have been
                                            handled in a different branch.*)
          (*Add "Return result_idx" to end of function body*)
          Seq (body', Return (Some (Var result_idx))) in
        (body'', st4) in

    let (fn_idx, st5) =
      if is_fresh fn_decl.fn_name st4 then
        extend_scope_unsafe Term st4 ~ty_opt:None(*FIXME put function's type here?*)
                          fn_decl.fn_name
      else
        failwith ("Function name " ^ fn_decl.fn_name ^ " isn't fresh.")
    in (Fun_Decl
          {
            id = fn_idx;
            arg_tys = n_arg_tys;
            ret_ty = n_res_ty;
            body =
              (*Initialise some data that might be needed during the final
                passes (inlining and variable erasure)*)
              let arg_idxs = List.map (fun x ->
                idx_of_naasty_type x
                |> the) n_arg_tys in
              (*Initialise table for the inliner.
                Mention the parameters in the initial table*)
              let init_table = Inliner.init_table arg_idxs in

              let inlined_body init_table st body =
                if !Config.cfg.Config.disable_inlining then body
                else
                  let subst = Inliner.mk_subst st init_table body in
                  (*If all variables mentioned in an assignment/declaration are to be deleted,
                    then delete the assignment/declaration*)
                  Inliner.erase_vars body (List.map fst subst)
                  (*Do the inlining*)
                  |> Inliner.subst_stmt subst in

              let var_erased_body init_table st body =
                if !Config.cfg.Config.disable_var_erasure then body
                else
                  Inliner.mk_erase_ident_list st init_table body
                  |> Inliner.erase_vars body

              in
                inlined_body init_table st5 body''
                |> var_erased_body init_table st5
          },
        st5)

  | Process process ->
    (*A process could be regarded as a unit-returning function that is evaluated
      repeatedly in a loop, until a stopping condition is satisfied. Perhaps the
      most common stopping condition will be "end of file" or "no more data"
      which could show up in the language as something like "the channel was
      terminated unexceptionally by the other party".*)
    (*FIXME!*)(Type_Decl (Bool_Type (Some (-1))), st)
  | Include filename ->
    failwith "'include' statements should have been expanded earlier"

let naasty_of_flick_program ?st:((st : state) = initial_state) (p : program) : (naasty_program * state) =
  fold_map ([], st) naasty_of_flick_toplevel_decl p
