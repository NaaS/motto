(*
   Support for translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, October 2015
*)

open General
open Crisp_syntax
open Crisp_syntax_aux

type local_name_map = (label * label) list

let apply_endomap_opt lmn label_opt =
  bind_opt (fun label -> Some (apply_endomap lmn label)) None label_opt

(*Apply the local_name_map to expressions/types/etc. This is necessary before
 giving the expression to any functions that will perform lookups on variables,
 if those variables are in the domain of local_name_map, otherwise those lookups
 might take place on incorrect or non-existent symbol names.*)


let rec apply_lnm_ty (lmn : local_name_map) (ty : type_value) : type_value =
  match ty with
  | UserDefinedType (label_opt, type_name) ->
    UserDefinedType (apply_endomap_opt lmn label_opt,
                     apply_endomap lmn type_name)
  | IPv4Address label_opt ->
    IPv4Address (apply_endomap_opt lmn label_opt)

  | String (label_opt, ty_ann) ->
    String (apply_endomap_opt lmn label_opt, ty_ann)
  | Integer (label_opt, ty_ann) ->
    Integer (apply_endomap_opt lmn label_opt, ty_ann)
  | Boolean (label_opt, ty_ann) ->
    Boolean (apply_endomap_opt lmn label_opt, ty_ann)
  | Reference (label_opt, ty) ->
    Reference (apply_endomap_opt lmn label_opt, apply_lnm_ty lmn ty)

  | RecordType (label_opt, tys, ty_ann) ->
    RecordType (apply_endomap_opt lmn label_opt,
                List.map (apply_lnm_ty lmn) tys, ty_ann)

  | Disjoint_Union (label_opt, tys) ->
    Disjoint_Union (apply_endomap_opt lmn label_opt,
                    List.map (apply_lnm_ty lmn) tys)

  | List (label_opt, ty, di_opt, ty_ann) ->
    List (apply_endomap_opt lmn label_opt,
          apply_lnm_ty lmn ty,
          apply_endomap_opt lmn di_opt,
          ty_ann)

  | Dictionary (label_opt, ty1, ty2) ->
    Dictionary (apply_endomap_opt lmn label_opt,
                apply_lnm_ty lmn ty1,
                apply_lnm_ty lmn ty2)

  | Tuple (label_opt, tys) ->
    Tuple (apply_endomap_opt lmn label_opt,
           List.map (apply_lnm_ty lmn) tys)

  | ChanType (label_opt, ct) ->
    ChanType (apply_endomap_opt lmn label_opt,
              apply_lnm_channel_type lmn ct)

  | Undefined _
  | Empty
  | IL_Type _ -> ty

and apply_lnm_channel_type (lnm : local_name_map) (ct : channel_type) : channel_type =
  match ct with
  | ChannelSingle (ty1, ty2) ->
    ChannelSingle (apply_lnm_ty lnm ty1, apply_lnm_ty lnm ty2)
  | ChannelArray (ty1, ty2, di_opt) ->
    ChannelArray (apply_lnm_ty lnm ty1, apply_lnm_ty lnm ty2,
                  apply_endomap_opt lnm di_opt)

and apply_lnm_e (lmn : local_name_map) (e : expression) : expression =
  match e with
  | Can e' -> Can (apply_lnm_e lmn e')
  | True
  | False
  | Bottom
  | Int _
  | IPv4_address _
  | EmptyList
  | Str _
  | Meta_quoted _
  | Hole -> e

  | InvertedVariable label -> InvertedVariable (apply_endomap lmn label)
  | Variable label -> Variable (apply_endomap lmn label)
  | TypeAnnotation (e, ty) ->
    TypeAnnotation (apply_lnm_e lmn e, apply_lnm_ty lmn ty)
  | LocalDef ((value_name, ty_opt), e') ->
    LocalDef ((apply_endomap lmn value_name,
               bind_opt (fun ty -> Some (apply_lnm_ty lmn ty)) None ty_opt),
              apply_lnm_e lmn e)

  | Not e' ->
    Not (apply_lnm_e lmn e')
  | Abs e' ->
    Abs (apply_lnm_e lmn e')
  | Int_to_address e' ->
    Int_to_address (apply_lnm_e lmn e')
  | Address_to_int e' ->
    Address_to_int (apply_lnm_e lmn e')

  | And (e1, e2) ->
    And (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Or (e1, e2) ->
    Or (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Equals (e1, e2) ->
    Equals (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | GreaterThan (e1, e2) ->
    GreaterThan (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | LessThan (e1, e2) ->
    LessThan (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Plus (e1, e2) ->
    Plus (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Minus (e1, e2) ->
    Minus (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Times (e1, e2) ->
    Times (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Mod (e1, e2) ->
    Mod (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Quotient (e1, e2) ->
    Quotient (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | ConsList (e1, e2) ->
    ConsList (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | AppendList (e1, e2) ->
    AppendList (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Seq (e1, e2) ->
    Seq (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | Par (e1, e2) ->
    Par (apply_lnm_e lmn e1, apply_lnm_e lmn e2)
  | IntegerRange (e1, e2) ->
    IntegerRange (apply_lnm_e lmn e1, apply_lnm_e lmn e2)

  | ITE (e1, e2, e3_opt) ->
    ITE (apply_lnm_e lmn e1, apply_lnm_e lmn e2,
         bind_opt (fun e -> Some (apply_lnm_e lmn e)) None e3_opt)
  | TupleValue es ->
    TupleValue (List.map (apply_lnm_e lmn) es)

  | Update (value_name, e) ->
    Update (apply_endomap lmn value_name, apply_lnm_e lmn e)
  | UpdateIndexable (value_name, e1, e2) ->
    UpdateIndexable (value_name, apply_lnm_e lmn e1, apply_lnm_e lmn e2)

  | RecordProjection (e, label) ->
    RecordProjection (apply_lnm_e lmn e, apply_endomap lmn label)
  | Functor_App (function_name, fun_args) ->
    Functor_App (apply_endomap lmn function_name,
                 List.map (apply_lnm_funargs lmn) fun_args)
  | Record fields ->
    let fields' =
      List.map (fun (label, e) ->
        (apply_endomap lmn label, apply_lnm_e lmn e)) fields in
    Record fields'
  | RecordUpdate (e1, (label, e2)) ->
    RecordUpdate (apply_lnm_e lmn e1, (apply_endomap lmn label, apply_lnm_e lmn e2))
  | CaseOf (e, cases) ->
    let cases' =
      List.map (fun (e1, e2) ->
        (apply_lnm_e lmn e1, apply_lnm_e lmn e2)) cases in
    CaseOf (apply_lnm_e lmn e, cases')
  | IndexableProjection (label, e) ->
    IndexableProjection (apply_endomap lmn label, apply_lnm_e lmn e)
  | Map (label, e1, e2, b) ->
    Map (apply_endomap lmn label, apply_lnm_e lmn e1, apply_lnm_e lmn e2, b)
  | Iterate (label, e1, acc_opt, body, b) ->
    Iterate (apply_endomap lmn label, apply_lnm_e lmn e1,
             bind_opt (fun (l, e) ->
               Some (apply_endomap lmn l, apply_lnm_e lmn e)) None acc_opt,
             apply_lnm_e lmn body, b)
  | Send (b, ci, e) ->
    Send (b, apply_lnm_ci lmn ci, apply_lnm_e lmn e)
  | Receive (b, ci) ->
    Receive (b, apply_lnm_ci lmn ci)
  | Peek (b, ci) ->
    Peek (b, apply_lnm_ci lmn ci)

and apply_lnm_ci (lnm : local_name_map) ((cn, e_opt) : channel_identifier) : channel_identifier =
  (apply_endomap lnm cn,
   bind_opt (fun e -> Some (apply_lnm_e lnm e)) None e_opt)

and apply_lnm_funargs lmn : fun_arg -> fun_arg = function
  | Exp e -> Exp (apply_lnm_e lmn e)
  | Named (label, e) -> Named (apply_endomap lmn label, apply_lnm_e lmn e)
