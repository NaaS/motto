(*
   Templates related to the data model designed by Richard Clegg.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open General
open State
open Crisp_syntax
open Naasty

type data_model_component =
  { name : string;
    identifiers : string list;
    type_scheme : naasty_type;
    function_scheme : naasty_function }

type type_analysis =
  Naasty.naasty_statement list * string list * identifier

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
let get_channel_len (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let name = "get_channel_len" in
  let identifiers =
    [name;
     datatype_name ^ "::get_channel_len";
     "len";
     datatype_name;
     "sizeof";
    ] in
  let rec analyse_type ty ((stmts, names, next_placeholder) as acc : type_analysis) : type_analysis =
    match ty with
    | RecordType (label_opt, tys, ty_ann) ->
      (*FIXME probably we should look at ty_ann*)
      List.fold_right analyse_type tys acc
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
                If1 (GEq (Var next_placeholder, Int_Value 0),
                     Increment (-3, Var next_placeholder))
              in (stmt :: stmts, s :: names, next_placeholder - 1)
          end
        | _ -> failwith "Too many sizes specified for a string."
      end
    | _ -> acc in
  let body_contents, more_idents, _ =
    analyse_type ty
      ((*The initial program does nothing*)
        [Skip],
       (*Initially we haven't accumulated any list of names*)
        [],
       (*The next place holder used will depend on how many identifiers we have
         so far*)
        (List.length identifiers + 1) * (-1)) in
  { name = name;
    identifiers = identifiers @ more_idents;
    type_scheme = Fun_Type (-1, Size_Type None, []);
    function_scheme =
      let fun_name_idx = -2 in
      let arg_tys = [] in
      let ret_ty = Size_Type None in
      let body =
        [
          Declaration (Size_Type (Some (-3)));
          Assign (-3, Call_Function (-5, [Var (-4)]));
          Naasty_aux.concat body_contents;
          Return (Var (-3))
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let get_stream_len (datatype_name : string) (ty : Crisp_syntax.type_value) =
  { name = "get_stream_len";
    identifiers =
      ["get_stream_len";
       datatype_name ^ "::get_stream_len";
      ];
    type_scheme = Fun_Type (-1, Size_Type None, []);
    function_scheme =
      let fun_name_idx = -2 in
      let arg_tys = [] in
      let ret_ty = Size_Type None in
      let body =
        [
          (*FIXME fill in the rest of the body*)
          Skip
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let bytes_stream_to_channel (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let ret_ty =
    Static_Type (None,
                 Reference_Type (None,
                                 UserDefined_Type (None, -2))) in
  let arg_tys =
    [Reference_Type (Some (-3), Char_Type None);
     Reference_Type (Some (-4), Char_Type None);
     Reference_Type (Some (-5), Char_Type None);
     Reference_Type (Some (-6), Size_Type None);
     Reference_Type (Some (-7), Size_Type None)] in
  { name = "bytes_stream_to_channel";
    identifiers =
      ["bytes_stream_to_channel";
       datatype_name;
       "stream";
       "channel";
       "streamend";
       "bytes_read";
       "bytes_written";
       datatype_name ^ "::bytes_stream_to_channel";
      ];
    type_scheme =
      Fun_Type (-1,
                ret_ty,
                arg_tys);
    function_scheme =
      let fun_name_idx = -8 in
      let body =
        [
          (*FIXME fill in the rest of the body*)
          Skip
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let write_bytes_to_channel (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let ret_ty = Static_Type (None, Unit_Type) in
  let arg_tys =
    [Reference_Type (None, UserDefined_Type (None, -2));
     Reference_Type (Some (-3), Char_Type None);
     Reference_Type (Some (-4), Size_Type None)] in
  { name = "write_bytes_to_channel";
    identifiers =
      ["write_bytes_to_channel";
       datatype_name;
       "channel";
       "no_bytes";
       datatype_name ^ "::write_bytes_to_channel";
      ];
    type_scheme =
      Fun_Type (-1,
                ret_ty,
                arg_tys);
    function_scheme =
      let fun_name_idx = -5 in
      let body =
        [
          (*FIXME fill in the rest of the body*)
          Skip
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let bytes_channel_to_stream (datatype_name : string) (ty : Crisp_syntax.type_value) =
  let ret_ty = Static_Type (None, Unit_Type) in
  let arg_tys =
    [Reference_Type (Some (-2), Char_Type None);
     Reference_Type (Some (-3), Char_Type None);
     Reference_Type (Some (-4), Size_Type None);
     Reference_Type (Some (-5), Size_Type None)] in
  { name = "bytes_channel_to_stream";
    identifiers =
      ["bytes_channel_to_stream";
       "stream";
       "channel";
       "bytes_read";
       "bytes_written";
       datatype_name ^ "::bytes_channel_to_stream";
      ];
    type_scheme =
      Fun_Type (-1,
                ret_ty,
                arg_tys);
    function_scheme =
      let fun_name_idx = -6 in
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
let instantiate_data_model (datatype_name : string) (ty : Crisp_syntax.type_value) =
  [get_channel_len datatype_name ty;
   get_stream_len datatype_name ty;
   bytes_stream_to_channel datatype_name ty;
   write_bytes_to_channel datatype_name ty;
   bytes_channel_to_stream datatype_name ty]
