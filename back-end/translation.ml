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
open Task_model
open State_aux

let log m =
  print_endline (Printf.sprintf "\027[36m %s\027[m%!" m)

let require_annotations = false

let unidir_chan_receive_suffix = "_receive_"
let unidir_chan_send_suffix = "_send_"

type local_name_map = (label * label) list

exception Translation_Type_Exc of string * type_value
exception Translation_Expr_Exc of
    string * expression option * local_name_map option *
    naasty_statement option * state

(*default_label is used when translating type declarations*)
let rec naasty_of_flick_type ?default_ik:(default_ik : identifier_kind option = None)
          ?default_label:(default_label : string option = None) (st : state) (ty : type_value) : (naasty_type * state) =
  let check_and_resolve_typename type_name =
    match lookup_name Type st type_name with
    | None -> failwith ("Undeclared type: " ^ type_name)
    | Some i -> i in
  let check_and_generate_typename typename_opt =
    let type_name =
      match typename_opt with
      | None ->
        begin
        match default_label with
        | None -> failwith "Was expecting type name."
        | Some label -> label
        end
      | Some type_name -> type_name in
    begin
      match lookup_name Type st type_name with
      | None -> extend_scope_unsafe Type st type_name
      | Some idx ->
        if forbid_shadowing then
          failwith ("Already declared type: " ^ type_name)
        else
          (idx, st)
    end in
  let check_and_generate_name (ik : identifier_kind) (ty : type_value)
        label_opt (st : state) =
    match label_opt with
    | None -> (None, st)
    | Some identifier ->
      begin
        match lookup_name (Term ik) st identifier with
        | None ->
          let (idx, st') =
            extend_scope_unsafe ~src_ty_opt:(Some ty) (Term ik) st identifier
          in (Some idx, st')
        | Some idx ->
          if forbid_shadowing then
            failwith ("Already declared identifier: " ^ identifier)
          else
            (Some idx, st)
      end in
  (*Register the naasty_type of a symbol in the symbol table*)
  let update_naasty_ty (ik : identifier_kind) (naasty_ty : naasty_type)
        label_opt (st : state) =
    match label_opt with
    | None -> st
    | Some identifier ->
      begin
        match lookup_name (Term ik) st identifier with
        | None ->
          st
          (* FIXME or do the following?
          failwith ("Cannot update non-existing symbol record for '" ^ identifier ^ "'") *)
        | Some idx ->
          let term_symbols' = List.map (fun ((id, index, metadata) as symbol_data) ->
            assert ((id = identifier && idx = index) ||
                    (id <> identifier && idx <> index));
            if index = idx then
              begin
                (*Usually we apply this after the symbol record has just been
                  created, and the naasty_type isn't set at creation time,
                  therefore it should still be None.
                assert (metadata.naasty_type = None);
                NOTE disabled this assertion since the assumption doesn't seem
                     valid*)
                let metadata' =
                  { metadata with
                    naasty_type = Some naasty_ty } in
                (id, index, metadata')
              end
            else symbol_data) st.term_symbols in
          { st with term_symbols = term_symbols' }
      end in
  match ty with
  | Undefined _ -> failwith "Cannot translate undefined type"
  | Disjoint_Union (_, _) -> failwith "Unsupported"
  | List (_, _, _, _) ->
    (*Lists can be turned into arrays*)
    failwith "Unsupported"
  | Dictionary (label_opt, idx_ty, type_name) ->
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
    let (label_opt', st') = check_and_generate_name Value ty label_opt st in
    let ty' = UserDefined_Type (label_opt', type_name') in
    let st' = update_naasty_ty Value ty' label_opt st'
    in (ty', st')
  | Boolean (label_opt, type_ann) ->
    if (type_ann <> []) then
      failwith "Boolean serialisation annotation not supported"; (*TODO*)
    let ik =
      match default_ik with
      | None -> Value
      | Some ik -> ik in
    let (label_opt', st') = check_and_generate_name ik ty label_opt st in
    let translated_ty = Bool_Type label_opt' in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx translated_ty (Term ik) st' in
    let st'' = update_naasty_ty ik translated_ty label_opt st''
    in (translated_ty, st'')
  | Integer (label_opt, type_ann) ->
    let ik =
      match default_ik with
      | None -> Value
      | Some ik -> ik in
    let (label_opt', st') = check_and_generate_name ik ty label_opt st in
    let translated_ty, st' =
      if not require_annotations then
        (Int_Type (label_opt', default_int_metadata), st)
      else
        if type_ann = [] then
          raise (Translation_Type_Exc ("No annotation given", ty))
        else
          let (label_opt', st') = check_and_generate_name ik ty label_opt st' in
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
        (Int_Type (label_opt', metadata), st') in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx translated_ty (Term ik) st' in
    let st'' = update_naasty_ty ik translated_ty label_opt st''
    in (translated_ty, st'')
  | IPv4Address label_opt ->
    let ik =
      match default_ik with
      | None -> Value
      | Some ik -> ik in
    let (label_opt', st') = check_and_generate_name ik ty label_opt st in
    let metadata = { signed = false; precision = 32; hadoop_vint = false } in
    let translated_ty = Int_Type (label_opt', metadata) in
    let st'' =
      match label_opt' with
      | None -> st'
      | Some idx ->
        update_symbol_type idx translated_ty (Term ik) st' in
    let st'' = update_naasty_ty ik translated_ty label_opt st''
    in (translated_ty, st'')
  | String (label_opt, type_ann) ->
    let ik =
      match default_ik with
      | None -> Value
      | Some ik -> ik in
    let (label_opt', st') = check_and_generate_name ik ty label_opt st in
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
        update_symbol_type idx container_type (Term ik) st' in
    let st'' = update_naasty_ty ik container_type label_opt st''
    in (container_type, st'')
  | Reference (label_opt, ty) ->
    let ik =
      match default_ik with
      | None -> Value
      | Some ik -> ik in
    let (label_opt', st') = check_and_generate_name ik ty label_opt st in
    let (ty', st'') = naasty_of_flick_type st' ty in
    let translated_ty = Pointer_Type (label_opt', ty') in
    let st''' =
      match label_opt' with
      | None -> st''
      | Some idx ->
        update_symbol_type idx translated_ty (Term ik) st'' in
    let st''' = update_naasty_ty ik translated_ty label_opt st'''
    in (translated_ty, st''')
  | RecordType (label_opt, tys, type_ann) ->
    if (type_ann <> []) then
      failwith "Record serialisation annotation not supported"; (*TODO*)
    let (type_identifier, st') = check_and_generate_typename label_opt in
    let (tys', st'') =
      fold_map ([], st') (naasty_of_flick_type ~default_ik:(Some (Field ty))) tys in
    let translated_ty = Record_Type (type_identifier, List.rev tys') in
    let st''' =
        update_symbol_type type_identifier translated_ty Type st''
    in (translated_ty, st''')
  | ChanType (label_opt, chan_type) ->  (*FIXME -- THIS IS A DUMMY NOT REAL TRANSLATION *)
    let ik =
      match default_ik with
      | None -> Value
      | Some ik -> ik in
    let (naasty_label_opt, st) = check_and_generate_name ik ty label_opt st in
    match chan_type with  
    | ChannelSingle (in_type, out_type) -> 
      (match in_type with 
      | Empty -> 
        let (naasty_type, st) = naasty_of_flick_type st out_type in
        (Chan_Type (naasty_label_opt, false, Output, naasty_type),st)
      | _ -> 
        let (naasty_type, st) = naasty_of_flick_type st in_type in
        (Chan_Type (naasty_label_opt, false, Output, naasty_type),st)  )   
    | ChannelArray (in_type, out_type, dep_ind_opt) -> 
      (match in_type with 
      | Empty -> 
        let (naasty_type, st) = naasty_of_flick_type st out_type in
        (Chan_Type (naasty_label_opt, true, Output, naasty_type),st)
      | _ -> 
        let (naasty_type, st) = naasty_of_flick_type st in_type in
        (Chan_Type (naasty_label_opt, true, Output, naasty_type),st)  ) 

(*If the name exists in the name mapping, then map it, otherwise leave the name
  as it is*)
let try_local_name (l : label)
      (local_name_map : local_name_map) : label =
  try List.assoc l local_name_map
  with Not_found -> l
(*Add a local name: a mapping from a programmer-chosen name (within this scope)
  to the name that the compiler decided to use for this. The latter may be
  based on the former, but could be different in order to deal with shadowing.*)
let extend_local_names (local_name_map : local_name_map) (ik : identifier_kind)
      (name : label) (name' : identifier) (st : state) : (label * label) list =
  (name, resolve_idx (Term ik) no_prefix (Some st) name') :: local_name_map

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
             possibly extended local_name_map, (this is extended by LocalDef)
             possibly extended st
*)
(*FIXME fix the lift_assign idiom, to make sure that if assign_acc=[] then we
        still get the expression evaluated, since we potentially have
        side-effects in sub-expressions.*)
let rec naasty_of_flick_expr (st : state) (e : expression)
          (local_name_map : local_name_map)
          (sts_acc : naasty_statement) (ctxt_acc : identifier list)
          (assign_acc : identifier list) : (naasty_statement *
                                            identifier list (*ctxt_acc*) *
                                            identifier list (*assign_acc*) *
                                            local_name_map *
                                            state) =
  let check_and_resolve_name st identifier =
    try
      begin
        match lookup_name (Term Undetermined) st identifier with
        | None ->
          raise (Translation_Expr_Exc ("Undeclared identifier: " ^ identifier,
                                       Some e, Some local_name_map, Some sts_acc, st))
        | Some i -> i
      end
    with
    | State_Exc s ->
      raise (Translation_Expr_Exc ("State_Exc: " ^ s,
                                   Some e, Some local_name_map, Some sts_acc,
                                   st)) in
  try
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
          local_name_map,
          st)
    else
      let translated =
        try_local_name value_name local_name_map
        |> check_and_resolve_name st
        |> (fun x -> Var x) (*whither eta?*)
        |> lift_assign assign_acc
        |> Naasty_aux.concat
      in (mk_seq sts_acc translated, ctxt_acc,
          [](*Having assigned to assign_accs, we can forget them.*),
          local_name_map,
          st)
  | Seq (e1, e2) ->
    let (sts_acc', ctxt_acc', assign_acc', local_name_map', st') =
      (*We give the evaluation of e1 an empty assign_acc, since the assign_acc
        we received is intended for e2.*)
      naasty_of_flick_expr st e1 local_name_map sts_acc ctxt_acc []
    in
      assert (assign_acc' = []); (*We shouldn't be getting anything to assign
                                   from e1, particularly as we gave it an empty
                                   assign_acc*)
      naasty_of_flick_expr st' e2 local_name_map' sts_acc' ctxt_acc' assign_acc

  | True
  | False ->
      let translated =
        lift_assign assign_acc (Bool_Value (e = True))
        |> Naasty_aux.concat
      in
      (*having assigned to assign_acc, we've done our duty, so we return an
        empty assign_acc*)
      (mk_seq sts_acc translated, ctxt_acc, [], local_name_map, st)
  | And (e1, e2)
  | Or (e1, e2)
  | Equals (e1, e2) (*FIXME if e1 and e2 are strings then need to use strcmp.
                            different types may have different comparator syntax.*)
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
    let (_, e1_result_idx, st') = mk_fresh (Term Value) ~ty_opt:(Some ty1) "x_" 0 st in
    let (sts_acc', ctxt_acc', assign_acc', _, st'') =
      naasty_of_flick_expr st' e1 local_name_map sts_acc (e1_result_idx :: ctxt_acc)
        [e1_result_idx] in
    let (_, e2_result_idx, st''') =
      mk_fresh (Term Value) ~ty_opt:(Some ty2) "x_" e1_result_idx st'' in
    let (sts_acc'', ctxt_acc'', assign_acc'', _, st4) =
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
    (mk_seq sts_acc'' nstmt, ctxt_acc'', [], local_name_map, st4)
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
    let (_, e_result_idx, st') = mk_fresh (Term Value) ~ty_opt:(Some ty) "x_" 0 st in
    let (sts_acc', ctxt_acc', assign_acc', _, st'') =
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
    (mk_seq sts_acc' not_nstmt, ctxt_acc', [], local_name_map, st'')

  | Int i ->
    let translated = Int_Value i in
    let nstmt =
      lift_assign assign_acc translated
      |> Naasty_aux.concat
    in
    (mk_seq sts_acc nstmt, ctxt_acc, [], local_name_map, st)

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
          mk_fresh (Term Value) ~ty_opt:(Some acc_init_ty) (acc_label ^ "_") 0 st in
        let (sts_acc, ctxt_acc, assign_acc, _, st) =
          naasty_of_flick_expr st' acc_init local_name_map sts_acc
            (acc_label_idx :: ctxt_acc) [acc_label_idx] in
        ((sts_acc, ctxt_acc, assign_acc, st),
         extend_local_names local_name_map Value acc_label acc_label_idx st',
         Some acc_label_idx) in
    assert (assign_acc' = []);

    (*FIXME move this somewhere more general?*)
    (*FIXME must this be an integer -- use type inference?*)
    let int_ty = Int_Type (None, default_int_metadata) in

    (*Determine the "from" and "until" values of the loop.
      NOTE we ignore the value we'd get for assign_acc'', since this should be
           [] -- we ensure that this is so, and assert that intermediate values
           of this are [] too, as can be seen below.*)
    let (sts_acc'', ctxt_acc'', _, st'', from_idx, to_idx) =
      match range_e with
      | IntegerRange (from_e, until_e)->
        let (_, from_idx, st'') =
          mk_fresh (Term Value) ~ty_opt:(Some int_ty) "from_" 0 st' in
        let (sts_acc'', ctxt_acc'', assign_acc'', _, st''') =
          naasty_of_flick_expr st'' from_e local_name_map sts_acc'
            (from_idx :: ctxt_acc') [from_idx] in
        assert (assign_acc'' = []);

        let (_, to_idx, st4) =
          mk_fresh (Term Value) ~ty_opt:(Some int_ty) "to_" 0 st''' in
        let (sts_acc''', ctxt_acc''', assign_acc''', _, st5) =
          naasty_of_flick_expr st4 until_e local_name_map sts_acc''
            (to_idx :: ctxt_acc'') [to_idx] in
        assert (assign_acc''' = []);

        (sts_acc''', ctxt_acc''', [], st5, from_idx, to_idx)
      | _ -> failwith "Unsupported iteration range" in

    let (_, idx, st''') =
      mk_fresh (Term Value) ~ty_opt:(Some int_ty) (label ^ "_") 0 st'' in
    let idx_ty = the (lookup_symbol_type idx (Term Value) st''') in
    let (_, body_result_idx, st''') =
      mk_fresh (Term Value)
        ~ty_opt:(Some int_ty)(*FIXME this should be same as type of acc, since
                                     after all we'll be assigning this to acc*)
        ("body_result_") 0 st''' in

    let local_name_map'' =
      extend_local_names local_name_map' Value label idx st''' in

    let condition = LEq (Var from_idx, Var to_idx) in
    let increment = Increment (idx, Int_Value 1) in

    (*The tail statement is added to the body, it's the last statement executed
      in the body. It serves to update the acc, if one is being used.*)
    let tail_statement =
      match acc_label_idx_opt with
      | None -> Skip
      | Some acc_label_idx ->
        Assign (Var acc_label_idx, Var body_result_idx) in

    let (body, ctxt_acc''', assign_acc''', _, st4) =
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
    (Naasty_aux.concat [sts_acc''; for_stmt; nstmt], ctxt_acc''', [],
     local_name_map, st4)

  | Crisp_syntax.ITE (be, e1, e2_opt) -> 
     (*FIXME: if are expressions, so they should assume the value of either the then or the else branch, so 
       we should allocate a variable for that and assign the expression of one of the two to that variable *)
     (*FIXME currently assuming that e2_opt is defined; it may only be not given
       for unit-typed expressions, in which case we'll implicitly set the else
       branch to unity by using the one-handed If (If1) in NaaSty.*)
    let (_, cond_result_idx, st_before_cond) =
      mk_fresh (Term Value) ~ty_opt:(Some (Bool_Type None)) "ifcond_" 0 st in
    let (sts_acc_cond, ctxt_acc_cond, assign_acc_cond, _, st_cond) =
      naasty_of_flick_expr st_before_cond be local_name_map sts_acc (cond_result_idx :: ctxt_acc) [cond_result_idx] in
    assert (assign_acc_cond = []); (* We shouldn't get anything back to assign to *)
    let (_, if_result_idx, st_before_then) =
      (*FIXME here we use Int_Type, but this should be inferred from the if
        expression*)
      mk_fresh (Term Value)
        ~ty_opt:(Some
                  (*FIXME use type inference*)
                   (Int_Type (None, default_int_metadata))) "ifresult_" 0 st_cond in
    let (then_block, ctxt_acc_then, assign_acc_then, _, st_then) =
      naasty_of_flick_expr st_before_then e1 local_name_map Skip (if_result_idx :: ctxt_acc_cond) [if_result_idx] in
    let (else_block, ctxt_acc_else, assign_acc_else, _, st_else) =
      naasty_of_flick_expr st_then (the(*FIXME we assume the value's there*) e2_opt) local_name_map Skip ctxt_acc_then [if_result_idx] in
    let translated = 
      Naasty.If (Var cond_result_idx, then_block, else_block) in
    let nstmt =
      lift_assign assign_acc (Var if_result_idx)
      |> Naasty_aux.concat in
    (Naasty_aux.concat [sts_acc_cond; translated; nstmt], ctxt_acc_else,
     assign_acc_else, local_name_map, st_else);

  | IPv4_address (oct1, oct2, oct3, oct4) ->
    let addr =
      oct1 lsl 24 + oct2 lsl 16 + oct3 lsl 8 + oct4 in
    let translated = Int_Value addr in
    let nstmt =
      lift_assign assign_acc translated
      |> Naasty_aux.concat
    in
    (mk_seq sts_acc nstmt, ctxt_acc, [], local_name_map, st)
  | Int_to_address e
  | Address_to_int e ->
    (*We store IPv4 addresses as ints, therefore the conversion doesn't do anything.*)
    naasty_of_flick_expr st e local_name_map sts_acc ctxt_acc assign_acc

  | Functor_App (function_name, fun_args) ->
    (*Canonicalise the function's arguments -- eliminating any named-parameter
      occurrences.*)
    let arg_expressions = Crisp_syntax_aux.order_fun_args function_name st fun_args in

    let (dis, (chans, arg_tys), ret_tys) =
      List.assoc function_name st.State.crisp_funs
      |> snd (*FIXME disregarding whether function or process*)
      |> Crisp_syntax_aux.extract_function_types in
    assert (dis = []); (*FIXME currently functions cannot be dependent*)
    assert (chans = []); (*FIXME currently functions cannot be given channel
                           parameters*)

    (*Translate each function argument as an expression.*)
    let (result_indices, sts_acc', ctxt_acc', _, st') =
      List.fold_right (fun (e, ty') (result_indices, sts_acc, ctxt_acc, assign_acc, st) ->
        let ty, st = Type_infer.ty_of_expr ~strict:true st e in
        let ty =
          match Crisp_syntax_aux.type_unify ty ty' with
          | None ->
            raise (Translation_Expr_Exc
              ("Could not unify formal-parameter type with the inferred " ^
               "actual-parameter type argument of function '" ^ function_name ^ "'",
               Some e, Some local_name_map, None, st))
          | Some ty -> ty in
        let (naasty_ty, st') =
          naasty_of_flick_type st ty
             (*We need to do this step otherwise the variable will get declared
               with the name of the formal parameter, when we want it declared
               with the name of the fresh variable that we create next.*)
          |> apfst Naasty_aux.set_empty_identifier in
        let (_, e_result_idx, st'') =
          mk_fresh (Term Value) ~ty_opt:(Some naasty_ty) "funarg_" 0 st' in
        let (sts_acc', ctxt_acc', assign_acc', _, st''') =
          naasty_of_flick_expr st'' e local_name_map sts_acc (e_result_idx :: ctxt_acc)
            [e_result_idx] in
        (e_result_idx :: result_indices, sts_acc', ctxt_acc', assign_acc', st'''))
        (List.combine arg_expressions arg_tys |> List.rev)
        ([], sts_acc, ctxt_acc, assign_acc, st) in
    let result_indices = List.rev result_indices in

    let parameters = List.map (fun x -> Var x)(*whither eta?*) result_indices in

    let translated =
      let translated_expression =
        try_local_name function_name local_name_map
        |> check_and_resolve_name st'
        |> (fun x -> Call_Function (x, [], parameters)) in
      (*In case assign_acc = [], we still want the function to be called,
        otherwise we'll get bugs because programmer-intended side-effects
        don't get done.*)
      if assign_acc = [] then
        St_of_E translated_expression
      else
        lift_assign assign_acc translated_expression
        |> Naasty_aux.concat
    in (mk_seq sts_acc' translated, ctxt_acc',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st')

  | LocalDef ((name, ty_opt), e) ->
    let (naasty_ty, st') =
      match ty_opt with
      | None ->
        (*FIXME currently defaulting to Int, but we should use type inference
                on e here*)
        let ty = Integer (Some name, []) in
        let naasty_ty = Int_Type (None, default_int_metadata) in
        let (_, st) =
          (*FIXME should probably use mk_fresh*)
          extend_scope_unsafe (Term Value) st ~src_ty_opt:(Some ty) ~ty_opt:(Some naasty_ty) name
          (*FIXME should update the label info in the naasty_ty for this symbol;
                  currently only its Flick type mentions the identifier.*)
        in (naasty_ty, st)
      | Some ty ->
        let ty = Crisp_syntax_aux.update_empty_label name ty in
        let naasty_ty, st' = naasty_of_flick_type st ty in
(*        let (_, st'') =
          (*FIXME should probably use mk_fresh*)
          extend_scope_unsafe (Term Value) st' ~src_ty_opt:(Some ty) ~ty_opt:(Some naasty_ty) name
*)
        let st'' = st'

        in naasty_ty, st'' in
(*    FIXME the next line was "inlined" in the previous block, which contains
            that decides how to extend the symbol table.
      let (_, name_idx, st'') = mk_fresh (Term Value) ~ty_opt:(Some naasty_ty) name 0 st' in*)
      let st'' = st' in
      let name_idx =
        match lookup_name (Term Value) st' name with
        | None -> failwith ("Did not find index for symbol '" ^ name ^ "'")
        | Some idx -> idx in
      let (sts_acc', ctxt_acc', assign_acc', _, st''') =
        naasty_of_flick_expr st'' e local_name_map sts_acc (name_idx :: ctxt_acc)
          [name_idx] in
      assert (assign_acc' = []);
      let local_name_map' =
        extend_local_names local_name_map Value name name_idx st''' in
      (*The recursive call to naasty_of_flick_expr takes care of assigning to
        name_idx. Now we take care of assigning name_idx to assign_acc.*)
      let translated =
        lift_assign assign_acc (Var name_idx)
        |> Naasty_aux.concat
      in (mk_seq sts_acc' translated, ctxt_acc',
          [](*Having assigned to assign_accs, we can forget them.*),
          local_name_map',
          st''')

  | Update (value_name, expression) ->
    (*NOTE similar to how we handle LocalDef, except that we require that
           a suitable declaration took place earlier.*)
    (*FIXME check that expression is of a compatible type*)
    let name_idx =
      match lookup_name (Term Undetermined) st value_name with
      | None -> failwith ("Could not find previous declaration of " ^ value_name)
      | Some idx -> idx in
(*
    (*FIXME check that name is of a reference type*)
    let idx_ty =
      match lookup_symbol_type name_idx Term st with
      | None -> failwith ("Could not find type declared for " ^ value_name)
      | Some ty -> ty in
*)
    let (sts_acc', ctxt_acc', assign_acc', _, st') =
      naasty_of_flick_expr st e local_name_map sts_acc ctxt_acc [name_idx] in
    assert (assign_acc' = []);
    (*The recursive call to naasty_of_flick_expr takes care of assigning to
      name_idx. Now we take care of assigning name_idx to assign_acc.*)
    let translated =
      lift_assign assign_acc (Var name_idx)
      |> Naasty_aux.concat
    in (mk_seq sts_acc' translated, ctxt_acc',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st')

  | TupleValue es ->
    (*Each tuple could be a different type*)
    (*FIXME fuse equivalent tuple types -- this could be done during an early
              static analysis during compilation*)
    let (_, tuple_instance_ty_idx, st') =
      mk_fresh Type ~ty_opt:None "tupletype_" 0 st in
    let tuple_instance_ty =
      let component_tys =
        List.map (fun _ ->
          (*FIXME use type inference*)
          Int_Type (None, default_int_metadata)) es in
        Record_Type (tuple_instance_ty_idx, component_tys) in
    let (_, tuple_instance_idx, st'') =
      update_symbol_type tuple_instance_ty_idx tuple_instance_ty Type st'
      |> mk_fresh (Term Value)
           ~ty_opt:(Some (UserDefined_Type (None, tuple_instance_ty_idx))) "tuple_" 0 in

    (*NOTE this bit is similar to part of Function_Call*)
    let (result_indices, sts_acc', ctxt_acc', _, st''') =
      List.fold_right (fun e (result_indices, sts_acc, ctxt_acc, assign_acc, st) ->
        let naasty_ty =
          (*FIXME use type inference*)
          Int_Type (None, default_int_metadata) in
        let (_, e_result_idx, st') = mk_fresh (Term Value) ~ty_opt:(Some naasty_ty) "tuplefield_" 0 st in
        let (sts_acc', ctxt_acc', assign_acc', _, st'') =
          naasty_of_flick_expr st' e local_name_map sts_acc (e_result_idx :: ctxt_acc)
            [e_result_idx] in
        (e_result_idx :: result_indices, sts_acc', ctxt_acc', assign_acc', st''))
        es ([], sts_acc, ctxt_acc, assign_acc, st'') in
    let result_indices = List.rev result_indices in

    let tuple =
      Assign (Var tuple_instance_idx,
              Record_Value
                (List.map (fun idx -> (idx, Var idx)) result_indices)) in

    let translated =
      Var tuple_instance_idx
      |> lift_assign assign_acc
      |> Naasty_aux.concat
    in (Naasty_aux.concat [sts_acc'; tuple; translated],
        (*add declaration for the fresh name we have for this tuple instance*)
        tuple_instance_idx :: (*tuple_instance_ty_idx :: FIXME tuple type
                                declaration isn't being included anywhere*) ctxt_acc',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st''')

  | TypeAnnotation (e, _) ->
    (*NOTE could translate this into an explicit type-case, using the second
           parameter of TypeAnnotation*)
    naasty_of_flick_expr st e local_name_map sts_acc ctxt_acc assign_acc

  | RecordProjection (e, label) ->
    let naasty_ty =
      (*FIXME use type inference*)
      Int_Type (None, default_int_metadata) in
    let name_idx =
      match lookup_name (Term Undetermined) st label with
      | None -> failwith ("Could not find previous declaration of " ^ label)
      | Some idx -> idx in
    let (_, e_result_idx, st') =
      mk_fresh (Term Value) ~ty_opt:(Some naasty_ty) "record_" 0 st in
    let (sts_acc', ctxt_acc', assign_acc', _, st'') =
      naasty_of_flick_expr st' e local_name_map sts_acc
        (e_result_idx :: ctxt_acc) [e_result_idx] in
    assert (assign_acc' = []);
    let translated =
      Field_In_Record (Var e_result_idx, Var name_idx)
      |> lift_assign assign_acc
      |> Naasty_aux.concat
    in (Naasty_aux.concat [sts_acc'; translated],
        (*add declaration for the fresh name we have for this tuple instance*)
        ctxt_acc',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st'')

  | RecordUpdate (record, (field_label, field_body)) ->
    (*NOTE using in-place update*)
    let naasty_ty =
      (*FIXME use type inference*)
      Int_Type (None, default_int_metadata) in
    let field_idx =
      match lookup_name (Term Undetermined) st field_label with
      | None -> failwith ("Could not find previous declaration of " ^ field_label)
      | Some idx -> idx in
    let (_, record_idx, st') =
      mk_fresh (Term Value) ~ty_opt:(Some naasty_ty) "record_" 0 st in
    let (sts_acc', ctxt_acc', assign_acc', _, st'') =
      naasty_of_flick_expr st' record local_name_map sts_acc
        (record_idx :: ctxt_acc) [record_idx] in
    assert (assign_acc' = []);

    let field_body_ty =
      (*FIXME use type inference*)
      Int_Type (None, default_int_metadata) in
    let (_, field_body_idx, st''') =
      mk_fresh (Term Value) ~ty_opt:(Some field_body_ty) "fieldbody_" 0 st'' in
    let (sts_acc'', ctxt_acc'', assign_acc'', _, st4) =
      naasty_of_flick_expr st''' field_body local_name_map sts_acc'
        (field_body_idx :: ctxt_acc') [field_body_idx] in
    assert (assign_acc'' = []);

    (*FIXME should the percolated assignment be the value of the field, or the record?
            I'm going with record.*)

    let record_update =
      Assign (Field_In_Record (Var record_idx, Var field_idx), Var field_body_idx) in
    let translated =
      Var record_idx
      |> lift_assign assign_acc
      |> Naasty_aux.concat
    in (Naasty_aux.concat [sts_acc''; record_update; translated],
        (*add declaration for the fresh name we have for this tuple instance*)
        ctxt_acc'',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st4)

(*TODO
   list related, but constant:
   | Str s ->
   IntegerRange

   record-relate:
   Record

   disjunction
   CaseOf
   (update Functor_App to create well-formed union values in NaaSty)

   look-up related
   IndexableProjection
   UpdateIndexable

   list mapping
   Map

   lists -- must these be dynamic?
   EmptyList
   ConsList
   AppendList

   channel primitives:
     Exchange
*)
  | Receive (channel_inverted, channel_identifier) ->
    (*NOTE only data-model instances can be communicated via channels.
           also need to ensure that #include the data-model instance output*)
    let size, st' =
      Naasty_aux.add_symbol "size" (Term Value) ~ty_opt:(Some (Size_Type None)) st in
    let inputs, st'' =
      let ty =
        Array_Type (None,
                    (*FIXME should be the type of channel buffers*)
                    Int_Type (None, default_int_metadata),
                    (*FIXME should be the size of the channel array*)
                    Max 5) in
      Naasty_aux.add_symbol "inputs" (Term Value)
        (*FIXME carried-type of "inputs" array should be the channel type;
                this should be a parameter to the function/process*)
        ~ty_opt:(Some ty) st' in
    let consume_channel, st'' =
      (*FIXME should name-spacing ("NaasData") be hardcoded like this, or
              should it be left variable then resolved at compile time?*)
      Naasty_aux.add_symbol "NaasData::consume_channel" (Term Value)
        (*FIXME "consume_channel" should have some function type*)
        ~ty_opt:(Some
                  (*FIXME what type to use here -- use type inference?*)
                   (Int_Type (None, default_int_metadata))) st'' in
    (*FIXME code style here sucks*)
    let ctxt_acc' =
      if List.mem size ctxt_acc then ctxt_acc else size :: ctxt_acc in
    let ctxt_acc' =
      if List.mem inputs ctxt_acc' then ctxt_acc' else inputs :: ctxt_acc' in
    let (chan_name, opt) = channel_identifier in
    let (chan_index,chan_type) =  input_map chan_name st'' opt in
    let (_, chan_index_idx, st'') =
      mk_fresh (Term Value)
        (*Array indices are int-typed*)
        ~ty_opt:(Some (Int_Type (None, default_int_metadata)))
        ("chan_index_") 0 st'' in
    let translated =
      St_of_E (Call_Function
                 (consume_channel,
                  [Type_Parameter
                     (*FIXME need to work out the correct value for this*)
                     (Int_Type (None, default_int_metadata))],
                  [ArrayElement (Var inputs, Var chan_index_idx);
                   Address_of (Var size)]))
    (*FIXME we should lift_assign*)
    in (Naasty_aux.concat [sts_acc; translated],
        (*add declaration for the fresh name we have for this tuple instance*)
        chan_index_idx :: ctxt_acc',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st'')

  (*FIXME some code duplicated from "Receive"*)
  (*FIXME contains lots of default types -- usually "int" is used*)
  | Send (channel_inverted, channel_identifier, e) ->
    (*Start by translating e*)
    let e_ty = (*FIXME use type inference*)
      Int_Type (None, default_int_metadata) in
    (*Since this is a newly-declared variable, make sure it's fresh.*)
    let (_, e_idx, st') =
      mk_fresh (Term Value) ~ty_opt:(Some e_ty) ("e_") 0 st in
    let (sts_acc, ctxt_acc, assign_acc, _, st) =
      naasty_of_flick_expr st' e local_name_map sts_acc
        (e_idx :: ctxt_acc) [e_idx] in

    (*Add "te" declaration, unless it already exists in ctxt_acc*)
    let taskevent_ty, te, st' =
      Naasty_aux.add_usertyped_symbol "TaskEvent" "te" st in
    (*NOTE "size" value tells us how much data has been consumed. Should have
           distinct "size" variable for each occurrence of a channel-related
           function.
           However, since we don't ever read "size" in our use-cases at the
           moment, we always assign to the same "size", to avoid overhead of
           wasted space -- FIXME this optimisation could be done by the compiler.*)
    let size, st'' =
      Naasty_aux.add_symbol "size" (Term Value)
        (*FIXME type should be "size_t"*)
        ~ty_opt:(Some (Int_Type (None, default_int_metadata))) st' in
    let outputs, st'' =
      let ty =
        Array_Type (None, Int_Type (None, default_int_metadata), Max 5(*FIXME*)) in
      Naasty_aux.add_symbol "outputs" (Term Value)
        (*FIXME carried-type of "outputs" array should be the channel type;
                this should be a parameter to the function/process*)
        ~ty_opt:(Some ty) st'' in
    let write_channel, st'' =
      (*FIXME should name-spacing ("NaasData") be hardcoded like this, or
              should it be left variable then resolved at compile time?*)
      Naasty_aux.add_symbol "NaasData::write_to_channel" (Term Value)
        (*FIXME "write_channel" should have some function type*)
        ~ty_opt:(Some (Int_Type (None, default_int_metadata))) st'' in
    let ctxt_acc' =
      if List.mem te ctxt_acc then ctxt_acc else te :: ctxt_acc in
    (*FIXME code style here sucks*)
    let ctxt_acc' =
      if List.mem size ctxt_acc' then ctxt_acc' else size :: ctxt_acc' in
    let ctxt_acc' =
      if List.mem outputs ctxt_acc' then ctxt_acc' else outputs :: ctxt_acc' in
(*FIXME calculate channel offset*)
    let (chan_name, opt) = channel_identifier in
    let (chan_index,chan_type) =  output_map chan_name st'' opt in
    let (naasty_chan_type, st'')  = naasty_of_flick_type st'' chan_type  in
    let (_, chan_index_idx, st'') =
      mk_fresh (Term Value)
        (*Array indices are int-typed*)
        ~ty_opt:(Some (Int_Type (None, default_int_metadata)))
        ("consume_index_") 0 st'' in 
    let (sts_acc, ctx_acc', assign_acc', local_name_map, st'') = 
      naasty_of_flick_expr st'' chan_index local_name_map sts_acc ctxt_acc' [chan_index_idx] in
    let translated =
      Assign (Var te,
              (*FIXME how is value of "te" used?*)
              Call_Function (write_channel,
                             [Type_Parameter naasty_chan_type],
                             [Var e_idx;
                              ArrayElement (Var outputs, Var chan_index_idx);
                              Address_of (Var size)]))
    (*FIXME we should lift_assign*)
    in (Naasty_aux.concat [sts_acc; translated],
        (*add declaration for the fresh name we have for this tuple instance*)
        chan_index_idx :: ctxt_acc',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st'')

  (*FIXME need to improve inliner to avoid inlining function calls -- don't
          want to multiply their effects. (This doesn't matter in the case of
          peek_chan since it's pure, but still.)*)
  (*FIXME lots of code duplicated from Receive*)
  (*FIXME need to add the line of checking that follows a call to peek_channel*)
  | Peek (channel_inverted, channel_identifier) ->
    let inputs, st' =
      let ty =
        Array_Type (None,
                    (*FIXME should be the type of channel buffers*)
                    Int_Type (None, default_int_metadata),
                    (*FIXME should be the size of the channel array*)
                    Max 5) in
      Naasty_aux.add_symbol "inputs" (Term Value)
        (*FIXME carried-type of "inputs" array should be the channel type;
                this should be a parameter to the function/process*)
        ~ty_opt:(Some ty) st in
    let peek_channel, st'' =
      (*FIXME should name-spacing ("NaasData") be hardcoded like this, or
              should it be left variable then resolved at compile time?*)
      Naasty_aux.add_symbol "NaasData::peek_channel" (Term Value)
        (*FIXME "consume_channel" should have some function type*)
        ~ty_opt:(Some
                  (*FIXME what type to use here?*)
                   (Int_Type (None, default_int_metadata))) st' in
    (*FIXME code style here sucks*)
    let ctxt_acc' =
      if List.mem inputs ctxt_acc then ctxt_acc else inputs :: ctxt_acc in
    let (chan_name, opt) = channel_identifier in
    let (chan_index,chan_type) =  input_map chan_name st'' opt in
    let (_, chan_index_idx, st'') =
      mk_fresh (Term Value)
        (*Array indices are int-typed*)
        ~ty_opt:(Some (Int_Type (None, default_int_metadata)))
        ("chan_index_") 0 st'' in
 
    let (sts_acc, ctx_acc', assign_acc', local_name_map, st'') = 
      naasty_of_flick_expr st'' chan_index local_name_map sts_acc ctxt_acc' [chan_index_idx] in

    let naasty_ty =
      (*FIXME use type inference*)
      Int_Type (None, default_int_metadata) in
    let (_, peek_idx, st'') =
      mk_fresh (Term Value) ~ty_opt:(Some naasty_ty) "peek_" 0 st'' in

    let translated_peek =
      Assign (Var peek_idx,
              Call_Function
                 (peek_channel,
                  [Type_Parameter
                     (*FIXME need to work out the correct value for this*)
                     (Int_Type (None, default_int_metadata))],
                  [ArrayElement (Var inputs, Var chan_index_idx)])) in
    let condition = Equals (Var peek_idx, Nullptr) in
    let naasty_ty =
      (*FIXME use type inference*)
      Int_Type (None, default_int_metadata) in
    let ret_func, st'' = Naasty_aux.add_symbol "TaskEvent" ~ty_opt:(Some naasty_ty) (Term Value) st'' in
    let ret_type =
      Call_Function(ret_func, [],
        [Literal "TaskEvent::OUT_OF_DATA"(*FIXME const -- store centrally, as with Data_model_consts*);
         Var chan_index_idx]) in
    let result =
      Return (Some (ret_type)) in
    let peek_with_check =
             (* Add check for nullptr and possible return*)
             (* naasty_statement.If1 naasty.Equals (Var peek_idx, nullptr)  (return TaskEvent (OUT_OF_DATA, channel *)
      Seq (translated_peek, (If1 (condition,result))) in
    let assignments =
      Var peek_idx
      |> lift_assign assign_acc
      |> Naasty_aux.concat

    in (Naasty_aux.concat [sts_acc; peek_with_check; assignments],
        (*add declaration for the fresh name we have for this tuple instance*)
        chan_index_idx :: ret_func :: peek_idx :: ctxt_acc',
        [](*Having assigned to assign_accs, we can forget them.*),
        local_name_map,
        st'')

  | _ -> raise (Translation_Expr_Exc ("TODO: " ^ expression_to_string no_indent e,
                                      Some e, Some local_name_map, Some sts_acc, st))
  with
  | Translation_Expr_Exc (msg, e_opt, lnm_opt, sts_acc_opt, st') ->
    (*FIXME code based on that in Wrap_err*)
    begin
    if not !Config.cfg.Config.unexceptional then
      let e_s =
        match e_opt with
        | None -> ""
        | Some e ->
          "at expression:" ^ Crisp_syntax.expression_to_string
                               Crisp_syntax.min_indentation e ^ "\n" in
      let local_name_map_s =
        match lnm_opt with
        | None -> ""
        | Some lnm ->
          List.map (fun (l1, l2) -> l1 ^ " |-> " ^ l2) lnm
          |> Debug.print_list "  " in
      let sts_acc_s =
        match sts_acc_opt with
        | None -> ""
        | Some sts_acc ->
          "having so far translated:" ^
            string_of_naasty_statement ~st_opt:(Some st')
            2 sts_acc in
      print_endline
       ("Translation error: " ^ msg ^ "\n" ^
        e_s ^
        "local_name_map : " ^ local_name_map_s ^ "\n" ^
        sts_acc_s ^ "\n" ^
        "state :\n" ^
        State_aux.state_to_str ~summary_types:(!Config.cfg.Config.summary_types)
          true st')
    else ();
    raise (Translation_Expr_Exc ("(contained in)", Some e, Some local_name_map,
                                 Some sts_acc, st))
    end

(*Split a (possibly bidirectional) Crisp channel into a collection of
  unidirectional NaaSty channels.*)
let unidirect_channel (st : state) (Channel (channel_type, channel_name)) : naasty_type list * state =
  let subchan ty direction suffix is_array st =
    let ty', st' = naasty_of_flick_type st ty in
    let _, name_idx, st'' =
      mk_fresh (Term Value) ~ty_opt:(Some ty')
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
(* NOTE Dependencies aren't fully supported. We're increasing support for
        them in order to be able to use them, since we need them for the
        NaaS use-cases.
    assert (dependency = None);*)
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
let naasty_of_flick_function_expr_body (ctxt : Naasty.identifier list)
      (waiting : Naasty.identifier list) (init_statmt : naasty_statement)
      (flick_body : Crisp_syntax.expression) (local_name_map : local_name_map) (st : state) =
  let (body, ctxt', waiting', _(*FIXME do we care about local_name_map' ?*), st') =
    naasty_of_flick_expr st flick_body local_name_map
      init_statmt ctxt waiting in
  (*There shouldn't be any more waiting variables at this point, they should
    all have been assigned something.*)
  assert (waiting' = []);
  let body' =
    List.fold_right (fun idx stmt ->
      let ty =
        match lookup_symbol_type idx (Term Value)(*all ctxt symbols are term-level*)
                st' with
        | None ->
          raise (Translation_Expr_Exc ("Couldn't resolve type of ctxt idx " ^
                             string_of_int idx, Some flick_body,
                             Some local_name_map, Some init_statmt, st'))
        | Some ty -> ty
      in mk_seq (Declaration (ty, None)) stmt) ctxt' body
  in (body', st')

let split_io_channels f =
  let replace_channels f chans =
    let rec replace f =
      let rec get_name n chans dir =
        match chans with
        | [] ->
          (*FIXME make error more informative: dump stack*)
          failwith ("Couldn't find channel name: " ^ n)
        | (Channel (ctype, cname))::_ when cname = n || cname = (n ^ "_" ^ dir) -> cname
        | _::t -> get_name n t dir
      in
      match f with
      | Bottom
      | True
      | False
      | InvertedVariable _
      | Variable _
      | Int _
      | IPv4_address _
      | EmptyList
      | Str _
      | Meta_quoted _
      | Hole ->
        f
      | Send (inv, (c_name, idx_opt), e) ->
        let name = get_name c_name chans "out" in
        let idx_opt' =
          match idx_opt with
          | None -> None
          | Some e -> Some (replace e)
        in
        Send (inv, (name, idx_opt'), replace e)
      | Receive (inv, (c_name, idx_opt)) ->
        let name = get_name c_name chans "in" in
        let idx_opt' =
          match idx_opt with
          | None -> None
          | Some e -> Some (replace e)
        in
        Receive (inv, (name, idx_opt'))
      | Peek (inv, (c_name, idx_opt)) ->
        let name = get_name c_name chans "in" in
        let idx_opt' =
          match idx_opt with
          | None -> None
          | Some e -> Some (replace e)
        in
        Peek (inv, (name, idx_opt'))
      | TypeAnnotation (e, ty) -> TypeAnnotation (replace e, ty) 
      | And (b1, b2) -> And (replace b1, replace b2)
      | Or (b1, b2) -> Or (replace b1, replace b2)
      | Not b' -> Not (replace b')
      | Equals (e1, e2) -> Equals (replace e1, replace e2)
  
      | GreaterThan (a1, a2) -> GreaterThan (replace a1, replace a2)
      | LessThan (a1, a2) -> LessThan (replace a1, replace a2)
  
      | Plus (a1, a2) -> Plus (replace a1, replace a2)
      | Minus (a1, a2) -> Minus (replace a1, replace a2)
      | Times (a1, a2) -> Times (replace a1, replace a2)
      | Mod (a1, a2) -> Mod (replace a1, replace a2)
      | Quotient (a1, a2) -> Quotient (replace a1, replace a2)
      | Abs a -> Abs (replace a)
  
      | Int_to_address e -> Int_to_address (replace e)
      | Address_to_int e -> Address_to_int (replace e)
  
      | ConsList (x, xs) -> ConsList (replace x, replace xs)
      | AppendList (xs, ys) -> AppendList (replace xs, replace ys)
  
      | TupleValue xs -> TupleValue (List.map (fun x -> replace x) xs) 
  
      | Seq (e1, e2) -> Seq (replace e1, replace e2)
      | Par (e1, e2) -> Par (replace e1, replace e2)
      | ITE (be, e1, e2_opt) ->
        let e2_opt' =
          match e2_opt with
          | None -> None
          | Some e -> Some (replace e)
        in
        ITE (replace be, replace e1, e2_opt')
      | LocalDef (ty, e) -> LocalDef (ty, replace e)
      | Update (v, e) -> Update (v, replace e)
      | UpdateIndexable (v, idx, e) -> UpdateIndexable (v, replace idx, replace e)
  
      | RecordProjection (e, l) -> RecordProjection (replace e, l)
  
      | Functor_App (f, es) ->
        let es' = List.map (fun arg -> 
          match arg with
          | Exp e -> Exp (replace e)
          | Named (l, e) -> Named (l, replace e)
          ) es in
        Functor_App (f, es')
  
      | Record es -> Record (List.map (fun (l, e) -> (l, replace e)) es)
  
      | RecordUpdate (r, (l, e)) -> RecordUpdate (replace r, (l, replace e)) 
  
      | CaseOf (e, es) ->
        CaseOf (replace e,
        (List.map (fun (e1, e2) -> (replace e1, replace e2)) es) )
  
      | IndexableProjection (v, idx) -> IndexableProjection (v, replace idx) 
  
      | IntegerRange (e1, e2) -> IntegerRange (replace e1, replace e2)
  
      | Map (v, l, body, unordered) -> Map (v, replace l, replace body, unordered)

      | Iterate (v, l, acc, body, unordered) ->
        let acc' =
          match acc with
          | None -> None
          | Some (acc_v, acc_e) -> Some (acc_v, replace acc_e)
        in
          Iterate (v, replace l, acc', replace body, unordered)
    in
      replace f
  in
  let rec split channels =
    match channels with
    | [] -> []
    | (Channel (ctype, cname))::t ->
      print_endline (channel_type_to_string ctype);
      match ctype with
      | ChannelSingle (Empty, _)
      | ChannelSingle (_, Empty)
      | ChannelArray (Empty, _, _)
      | ChannelArray (_, Empty, _) ->
        (Channel (ctype, cname))::(split t) 
      | ChannelSingle (v1, v2) ->
        (Channel (ChannelSingle (v1, Empty), cname ^ "_in"))::
        (Channel (ChannelSingle (Empty, v2), cname ^ "_out"))::(split t)
      | ChannelArray (v1, v2, dep) ->
        (Channel (ChannelArray (v1, Empty, dep), cname ^ "_in"))::
        (Channel (ChannelArray (Empty, v2, dep), cname ^ "_out"))::(split t)
  in  
  match f.fn_params with
  | FunType (dis, FunDomType (channels, values), ret) ->
    let channels' = split channels in
    let fn_body' =
      match f.fn_body with
      | ProcessBody (s, expression, e) ->
        ProcessBody (s, replace_channels expression channels', e)
    in
    { fn_name = f.fn_name;
      fn_params = FunType (dis, FunDomType (channels', values), ret);
      fn_body = fn_body';
    }

let rec naasty_of_flick_toplevel_decl (st : state) (tl : toplevel_decl) :
  (naasty_declaration * state) =
  match tl with
  | Type ty_decl ->
    let (ty', st') =
      Crisp_syntax_aux.update_empty_label ty_decl.type_name ty_decl.type_value
      |> naasty_of_flick_type st
    in (Type_Decl ty', st')
  | Function f ->
    let fn_decl = split_io_channels f in
    log (toplevel_decl_to_string (Function (split_io_channels f))); 
    (*FIXME might need to prefix function names with namespace, within the
            function body*)
    let (dis(*FIXME currently not doing anything with "dis"*), (chans, arg_tys), res_tys) =
      Crisp_syntax_aux.extract_function_types fn_decl.fn_params in
    (*local_name_map is local to function
     blocks, so we start out with an empty
     map when translating a function block.*)
    let local_name_map = [] in
    let (n_arg_tys, local_name_map, st') =
      let standard_types, (st', local_name_map) =
        fold_map ([], (st, local_name_map)) (fun (st, local_name_map) ty ->
          let l =
            match Crisp_syntax_aux.label_of_type ty with
            | None ->
              raise (Translation_Type_Exc ("Expected type to have label", ty))
            | Some l -> l in
          let naasty_ty, st = naasty_of_flick_type st ty in
          let _, id, st' =
            mk_fresh (Term Value) ~src_ty_opt:(Some ty) ~ty_opt:(Some naasty_ty) l 0 st in
          let lnm' = extend_local_names local_name_map Value l id st' in
          let naasty_ty' =
            set_empty_identifier naasty_ty
            |> update_empty_identifier id in
          naasty_ty', (st', lnm')) arg_tys in
      let channel_types, st'' =
        fold_map ([], st') unidirect_channel chans (*FIXME thread local_name_map*) in
      (List.flatten channel_types @ standard_types, local_name_map, st'') in

    let (n_res_ty, st'') =
      match res_tys with
      | [] -> (Unit_Type, st')
      | [res_ty] -> naasty_of_flick_type st' res_ty
      | _ ->
        (*FIXME restriction*)
        failwith "Currently only functions with single return type are supported." in

    (*FIXME other parts of the body (i.e, state and exceptions) currently are
            not being processed.*)
    let fn_expr_body =
      match fn_decl.fn_body with
      | ProcessBody (sts, e, excs) ->
        if sts <> [] || excs <> [] then
          failwith "Currently state and exceptions are not handled in functions"
        else e in

    let init_statmt = Skip in
    let body'', st4 =
      if n_res_ty = Unit_Type then
        let init_ctxt = [] in
        let init_assign_acc = [] in
        let body', st4 =
          naasty_of_flick_function_expr_body init_ctxt init_assign_acc init_statmt
            fn_expr_body local_name_map st'' in
        let body'' =
          (*Add "Return" to end of function body*)
          Seq (body', Return None)
        in (body'', st4)
      else
        let (_, result_idx, st''') =
          mk_fresh (Term Value) ~ty_opt:(Some n_res_ty) "result_" 0 st'' in
        (*Add type declaration for result_idx, which should be the same as res_ty
          since result_idx will carry the value that's computed in this function.*)
        let init_ctxt = [result_idx] in
        let init_assign_acc = [result_idx] in
        let body', st4 =
          naasty_of_flick_function_expr_body init_ctxt init_assign_acc init_statmt
            fn_expr_body local_name_map st''' in
        let body'' =
          assert (n_res_ty <> Unit_Type); (*Unit functions should have been
                                            handled in a different branch.*)
          (*Add "Return result_idx" to end of function body*)
          Seq (body', Return (Some (Var result_idx))) in
        (body'', st4) in

    let (fn_idx, st5) = (*FIXME code style here sucks*)
      match lookup_term_data (Term Function_Name) st4.term_symbols fn_decl.fn_name with
      | None ->
        failwith ("Function name " ^ fn_decl.fn_name ^ " not found in symbol table.")
      | Some (idx, _) -> (idx, st4) in
    (Fun_Decl
          {
            id = fn_idx;
            arg_tys = n_arg_tys;
            ret_ty = n_res_ty;
            body =
              (*Initialise table for the inliner and for variable erasure.
                Mention the parameters in the initial table*)
              let init_table =
                (*Initialise some data that might be needed during the final
                  passes (inlining and variable erasure)*)
                let arg_idxs = List.map (fun x ->
                  idx_of_naasty_type x
                  |> the) n_arg_tys in
                  Inliner.init_table arg_idxs in

              let _ =
                if !Config.cfg.Config.verbosity > 0 then
                  Inliner.table_to_string st5 init_table
                  |> (fun s -> print_endline ("Initial inliner table:" ^ s)) in

              let inlined_body init_table st body =
                if !Config.cfg.Config.disable_inlining then body
                else
                  let subst = Inliner.mk_subst st init_table body in
                  (*If all variables mentioned in an assignment/declaration are
                    to be deleted, then delete the assignment/declaration.
                    (Irrespective if it contains side-effecting functions,
                     since these have been moved elsewhere.)
                    FIXME this can re-order side-effecting functions, which
                          would lead to the introduction of bugs. Should check
                          to ensure that inlining doesn't reorder such
                          functions.*)
                  Inliner.erase_vars ~aggressive:true body (List.map fst subst)
                  (*Do the inlining*)
                  |> Inliner.subst_stmt subst in

              (*Iteratively delete variables that are never read, but
                don't delete what's assigned to them if it may contain
                side-effects (i.e., call erase_vars in non-aggressive mode).*)
              let rec var_erased_body init_table st body =
                if !Config.cfg.Config.disable_var_erasure then body
                else
                  let body' =
                    Inliner.mk_erase_ident_list st init_table body
                    |> Inliner.erase_vars body in
                  if body = body' then body
                  else var_erased_body init_table st body'
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
