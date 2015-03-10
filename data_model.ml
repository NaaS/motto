(*
   Templates related to the data model designed by Richard Clegg.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open General
open State
open Crisp_syntax
open Naasty
open Data_model_consts

type data_model_component =
  { name : string;
    identifiers : string list;
    type_scheme : naasty_type;
    function_scheme : naasty_function }

type type_analysis =
  Naasty.naasty_statement list * string list * identifier

module type Instance =
sig
  val instantiate_data_model : data_model_component list
end

module Instance (Values : Values) : Instance =
struct
open Values
(*
  Based on
   https://lsds.doc.ic.ac.uk/gitlab/naas/naas-box-system/blob/master/src/applications/hadoop_data_model/HadoopDMData.h 

  static HadoopDMData *bytes_stream_to_channel(char *stream, char *channel,
    char *streamend, size_t *bytes_read, size_t *bytes_written);
  static void write_bytes_to_channel(HadoopDMData *,char *chan,
    size_t *no_bytes);
  static void bytes_channel_to_stream(char *stream, char *channel,
    size_t *bytes_read, size_t *bytes_written);
  size_t get_channel_len();
  size_t get_stream_len();
*)
let rec analyse_type_getchannellen ty ((stmts, names, next_placeholder) as acc : type_analysis) : type_analysis =
  match ty with
  | RecordType (label_opt, tys, ty_ann) ->
    (*FIXME probably we should look at ty_ann*)
    List.fold_right analyse_type_getchannellen
      (List.rev tys)
      acc
  | String (label_opt, ty_ann) ->
    begin
      match List.filter (fun (k, v) -> k = "byte_size") ty_ann with
      | [] -> failwith "Strings need to be given an indication of size."
      | [(_, v)] ->
        begin
          match v with
          | Ann_Str _ -> failwith "TODO"
          | Ann_Int _ -> failwith "TODO"
          | Ann_Ident s ->
            let stmt =
              (*next_placeholder stands for the contents of "s"*)
              If1 (GEq (Var next_placeholder, Int_Value 0),
                   Increment (lenI, Var next_placeholder)) in
            let commented_stmt = Commented(stmt, "Handle '" ^ the label_opt ^ "'")
            in (commented_stmt :: stmts, s :: names, next_placeholder - 1)
        end
      | _ -> failwith "Too many sizes specified for a string."
    end
  | _ -> acc
let get_channel_len (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let body_contents, more_idents, _ =
    analyse_type_getchannellen ty
      ((*The initial program does nothing*)
        [Skip],
       (*Initially we haven't accumulated any list of names*)
        [],
       (*The next place holder used will depend on how many identifiers we have
         so far*)
        (List.length identifiers + 1) * (-1)) in
  { name = get_channel_lenK;
    identifiers = identifiers @ List.rev more_idents;
    type_scheme = Fun_Type (get_channel_lenI, Size_Type None, []);
    function_scheme =
      let fun_name_idx = datatype_gclI in
      let arg_tys = [] in
      let ret_ty = Size_Type None in
      let body =
        [
          Declaration (Size_Type (Some lenI), Some (Int_Value 0));
          Commented (Skip, "Length of fixed-length parts");
          Assign (Var lenI, Call_Function (sizeofI, [Var datatype_nameI]));
          Commented (Skip, "Length of variable-length parts");
          Naasty_aux.concat body_contents;
          Return (Var lenI)
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let rec analyse_type_getstreamlen ty ((stmts, names, next_placeholder) as acc : type_analysis) : type_analysis =
  match ty with
  | RecordType (label_opt, tys, ty_ann) ->
    (*FIXME probably we should look at ty_ann*)
    List.fold_right analyse_type_getstreamlen
      (List.rev tys)
      acc
  | Integer (label_opt, ty_ann) ->
    let naas_ty, st =
      Translation.naasty_of_flick_type
        initial_state (*FIXME instead of re-translating the type, could pass an
                        already-translated type value*)
        ty in
    let naas_ty' = Naasty_aux.set_empty_identifier naas_ty in
    let naas_ty_s =
      Naasty_aux.string_of_naasty_type ~st_opt:(Some st) Naasty_aux.no_indent naas_ty' in
    let is_hadoop_vint =
      List.exists (fun (k, v) -> k = "hadoop_vint"(*FIXME const*) &&
                                 v = Ann_Ident "true"(*FIXME const*)) ty_ann in
    let name, stmt =
      if is_hadoop_vint then
        (the label_opt,
         Increment (lenI,
                    Call_Function
                      (readWriteData_encodeVIntSizeI, [Var next_placeholder])))
      else
        (naas_ty_s,
         Increment (lenI, Call_Function (sizeofI, [Var next_placeholder]))) in
    let commented_stmt = Commented(stmt, "Handle '" ^ the label_opt ^ "'")
    in (commented_stmt :: stmts, name :: names, next_placeholder - 1)
  | _ -> acc
let get_stream_len (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let body_contents1, more_idents1, next_placeholder =
    analyse_type_getstreamlen ty
      ([Skip],
       [],
       (List.length identifiers + 1) * (-1)) in
  let body_contents2, more_idents2, _ =
    analyse_type_getchannellen ty
      ([Skip],
       [],
       next_placeholder) in
  { name = get_stream_lenK;
    identifiers = identifiers @ List.rev more_idents1 @ List.rev more_idents2;
    type_scheme = Fun_Type (get_stream_lenI, Size_Type None, []);
    function_scheme =
      let fun_name_idx = datatype_gslI in
      let arg_tys = [] in
      let ret_ty = Size_Type None in
      let body =
        [
          Declaration (Size_Type (Some lenI), Some (Int_Value 0));
          Commented (Skip, "Length of fixed-length parts");
          Naasty_aux.concat body_contents1;
          Commented (Skip, "Length of variable-length parts");
          Naasty_aux.concat body_contents2;
          Return (Var lenI)
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let rec analyse_type_bstc_static
          (target : identifier list)
          ty ((stmts, names, next_placeholder) as acc : type_analysis) : type_analysis =
  match ty with
  | RecordType (label_opt, tys, ty_ann) ->
    (*FIXME probably we should look at ty_ann*)
    (*FIXME accumulate target, in case we have nested records*)
    List.fold_right (analyse_type_bstc_static target)
      (List.rev tys)
      acc
  | Integer (label_opt, ty_ann) ->
    let name, name_idx = the label_opt, next_placeholder in
    let stmt =
      St_of_E
        (Call_Function
           (readWriteData_read_write_VIntI,
            [Address_of (Naasty_aux.nested_fields (name_idx :: target));
             Var streamI; Var streamendI; Address_of (Var read_offsetI)])) in
    let commented_stmt = Commented(stmt, "Handle '" ^ the label_opt ^ "'")
    in (commented_stmt :: stmts, name :: names, next_placeholder - 1)
  | _ -> acc
let rec analyse_type_bstc_dynamic
          (target : identifier list)
          ty ((stmts, names, next_placeholder) as acc : type_analysis) : type_analysis =
  match ty with
  | RecordType (label_opt, tys, ty_ann) ->
    (*FIXME probably we should look at ty_ann*)
    (*FIXME accumulate target, in case we have nested records*)
    List.fold_right (analyse_type_bstc_dynamic target)
      (List.rev tys)
      acc
  | String (label_opt, ty_ann) ->
    begin
      match List.filter (fun (k, v) -> k = "byte_size") ty_ann with
      | [] -> failwith "Strings need to be given an indication of size."
      | [(_, v)] ->
        begin
          match v with
          | Ann_Str _ -> failwith "TODO"
          | Ann_Int _ -> failwith "TODO"
          | Ann_Ident length_field ->
            let name, name_idx = the label_opt, next_placeholder in
            let length_field_idx = next_placeholder - 1 in
            let item_offset = Plus (Var channelI, Var write_offsetI) in
            let stmt1 =
              Assign (Naasty_aux.nested_fields (name_idx :: target),
                      item_offset) in
            let f_call =
              Call_Function
                (readWriteData_read_write_BytesI,
                 [item_offset;
                  Naasty_aux.nested_fields (length_field_idx :: target);
                  Var streamI;
                  Var streamendI;
                  Address_of (Var read_offsetI)]) in
            let stmt2 =
              If1 (GEq (Naasty_aux.nested_fields (length_field_idx :: target), Int_Value 0),
                   Increment (write_offsetI, f_call))
            in (stmt2 :: stmt1 :: stmts, length_field :: name :: names, next_placeholder - 2)
        end
      | _ -> failwith "Too many sizes specified for a string."
    end
  | _ -> acc
let bytes_stream_to_channel (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let param_data_ty identifier =
    Reference_Type (identifier, UserDefined_Type (None, datatype_nameI)) in
  let ret_ty = Static_Type (None, param_data_ty None) in
  let arg_tys =
    [Reference_Type (Some streamI, Char_Type None);
     Reference_Type (Some channelI, Char_Type None);
     Reference_Type (Some streamendI, Char_Type None);
     Reference_Type (Some bytes_readI, Size_Type None);
     Reference_Type (Some bytes_writtenI, Size_Type None)] in
  let body_contents1, more_idents1, next_placeholder =
    analyse_type_bstc_static [dataI] ty
      ([Skip],
       [],
       (List.length identifiers + 1) * (-1)) in
  let body_contents2, more_idents2, _ =
    analyse_type_bstc_dynamic [dataI] ty
      ([Skip],
       [],
       next_placeholder) in
  { name = bytes_stream_to_channelK;
    identifiers = identifiers @ List.rev more_idents1 @ List.rev more_idents2;
    type_scheme =
      Fun_Type (bytes_stream_to_channelI,
                ret_ty,
                arg_tys);
    function_scheme =
      let fun_name_idx = datatype_bstcI in
      let body =
        [
          Declaration (Size_Type (Some read_offsetI), Some (Int_Value 0));
          Declaration (Size_Type (Some write_offsetI), Some (Int_Value 0));
          Declaration (param_data_ty (Some dataI),
                       Some (Cast (param_data_ty None, channelI)));

          Commented (Skip, "Handling fixed-length data");
          Naasty_aux.concat body_contents1;
          Assign (Var write_offsetI,
                  Call_Function (sizeofI, [Var datatype_nameI]));

          Commented (Skip, "Handling variable-length data");
          Naasty_aux.concat body_contents2;

          Commented (Skip, "Update offsets");
          Assign (Dereference (Var bytes_readI), Var read_offsetI);
          Assign (Dereference (Var bytes_writtenI), Var write_offsetI);
          Return (Var dataI);
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let rec analyse_type_writebytestochannel_static
          (source : identifier list)
          (target : identifier list)
          ty
 ((stmts, names, next_placeholder) as acc : type_analysis) : type_analysis =
  match ty with
  | RecordType (label_opt, tys, ty_ann) ->
    (*FIXME probably we should look at ty_ann*)
    (*FIXME accumulate source and target, in case we have nested records*)
    List.fold_right (analyse_type_writebytestochannel_static source target)
      (List.rev tys)
      acc
  | Integer (label_opt, ty_ann) ->
    let source' = next_placeholder :: source in
    let target' = next_placeholder :: target in
    let stmt =
      Assign (Naasty_aux.nested_fields target', Naasty_aux.nested_fields source')
    in (stmt :: stmts, the label_opt :: names, next_placeholder - 1)
  | _ -> acc
let rec analyse_type_writebytestochannel_dynamic
          (source : identifier list)
          (target : identifier list)
          ty
 ((stmts, names, next_placeholder) as acc : type_analysis) : type_analysis =
  match ty with
  | RecordType (label_opt, tys, ty_ann) ->
    (*FIXME probably we should look at ty_ann*)
    (*FIXME accumulate source and target, in case we have nested records*)
    List.fold_right (analyse_type_writebytestochannel_dynamic source target)
      (List.rev tys)
      acc
  | String (label_opt, ty_ann) ->
    begin
      match List.filter (fun (k, v) -> k = "byte_size") ty_ann with
      | [] -> failwith "Strings need to be given an indication of size."
      | [(_, v)] ->
        begin
          match v with
          | Ann_Str _ -> failwith "TODO"
          | Ann_Int _ -> failwith "TODO"
          | Ann_Ident length_field ->
            let name, name_idx = the label_opt, next_placeholder in
            let length_field_idx = next_placeholder - 1 in
            let f_call =
              Call_Function (readWriteData_writeBytesI,
                             [Naasty_aux.nested_fields (name_idx :: source);
                              Naasty_aux.nested_fields (length_field_idx :: source);
                              Plus (Var channelI, Var offsetI)]) in
            let stmt =
              If1 (Gt (Naasty_aux.nested_fields (length_field_idx :: source), Int_Value 0),
                   Increment (offsetI, f_call))
            in (stmt :: stmts, name :: length_field :: names, next_placeholder - 2)
        end
      | _ -> failwith "Too many sizes specified for a string."
    end
  | _ -> acc

let write_bytes_to_channel (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let ret_ty = Static_Type (None, Unit_Type) in
  let param_data_ty identifier =
    Reference_Type (identifier, UserDefined_Type (None, datatype_nameI)) in
  let arg_tys =
    [param_data_ty (Some dataI);
     Reference_Type (Some channelI, Char_Type None);
     Reference_Type (Some no_bytesI, Size_Type None)] in
  let body_contents1, more_idents1, next_placeholder =
    analyse_type_writebytestochannel_static [dataI] [copyI] ty
      ([Skip],
       [],
       (List.length identifiers + 1) * (-1)) in
  let body_contents2, more_idents2, _ =
    analyse_type_writebytestochannel_dynamic [dataI] [copyI] ty
      ([Skip],
       [],
       next_placeholder) in
  { name = write_bytes_to_channelK;
    identifiers = identifiers @ List.rev more_idents1 @ List.rev more_idents2;
    type_scheme =
      Fun_Type (write_bytes_to_channelI,
                ret_ty,
                arg_tys);
    function_scheme =
      let fun_name_idx = datatype_wbtcI in
      let body =
        [
          Declaration (Size_Type (Some offsetI), Some (Int_Value 0));
          Declaration (param_data_ty (Some copyI),
                       Some (Cast (param_data_ty None, channelI)));
          Commented (Skip, "Handling fixed-length data");
          Naasty_aux.concat body_contents1;
          Assign (Var offsetI,
                  Plus (Var channelI,
                        Call_Function (sizeofI, [Var datatype_nameI])));
          Commented (Skip, "Handling variable-length data");
          Naasty_aux.concat body_contents2;
          Assign (Dereference (Var no_bytesI), Var offsetI);
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let bytes_channel_to_stream (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let ret_ty = Static_Type (None, Unit_Type) in
  let arg_tys =
    [Reference_Type (Some streamI, Char_Type None);
     Reference_Type (Some channelI, Char_Type None);
     Reference_Type (Some bytes_readI, Size_Type None);
     Reference_Type (Some bytes_writtenI, Size_Type None)] in
  { name = bytes_channel_to_streamK;
    identifiers = identifiers;
    type_scheme =
      Fun_Type (bytes_channel_to_streamI,
                ret_ty,
                arg_tys);
    function_scheme =
      let fun_name_idx = datatype_bctsI in
      let body =
        [
          (*FIXME fill in the rest of the body*)
          Skip
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

(*Instantiates the data model for a particular serialisable datatype.
  This should generate part of the "struct" definition in the resulting C++
  translation of the Flick type. *)
let instantiate_data_model =
  [get_channel_len Values.datatype_nameK Values.ty;
   get_stream_len Values.datatype_nameK Values.ty;
   bytes_stream_to_channel Values.datatype_nameK Values.ty;
   write_bytes_to_channel Values.datatype_nameK Values.ty;
   bytes_channel_to_stream Values.datatype_nameK Values.ty]
end
