(*
   Templates related to the data model designed by Richard Clegg.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open General
open State
open Crisp_syntax
open Naasty

(*FIXME this is used until the rest of the data model's function bodies are
  implemented*)
let dud_function = (-1, [], Unit_Type, Skip)

type data_model_component =
  { name : string;
    identifiers : string list;
    type_scheme : naasty_type;
    function_scheme : naasty_function }

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
  { name = name;
    identifiers =
      [name;
       datatype_name ^ "::get_channel_len";
       "x";
      ];
    type_scheme = Fun_Type (-1, Size_Type None, []);
    function_scheme =
      let fun_name_idx = -2 in
      let arg_tys = [] in
      let ret_ty = Size_Type None in
      let body =
        [
          Declaration (Size_Type (Some (-3)));
          Assign (-3, Int_Value 0);
          (*FIXME fill in the rest of the body*)
          Return (Var (-3))
        ] |> Naasty_aux.concat
      in (fun_name_idx, arg_tys, ret_ty, body);
  }

let get_stream_len (datatype_name : string) (ty : Crisp_syntax.type_value) =
  { name = "get_stream_len";
    identifiers = ["get_stream_len"];
    type_scheme = Fun_Type (-1, Size_Type None, []);
    function_scheme = dud_function;
  }

let bytes_stream_to_channel (datatype_name : string) (ty : Crisp_syntax.type_value) =
  { name = "bytes_stream_to_channel";
    identifiers =
      ["bytes_stream_to_channel";
       datatype_name;
       "stream";
       "channel";
       "streamend";
       "bytes_read";
       "bytes_written"];
    type_scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Reference_Type (None,
                                             UserDefined_Type (None, -2))),
                [Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Char_Type None);
                 Reference_Type (Some (-5), Char_Type None);
                 Reference_Type (Some (-6), Size_Type None);
                 Reference_Type (Some (-7), Size_Type None)]);
    function_scheme = dud_function;
  }

let write_bytes_to_channel (datatype_name : string) (ty : Crisp_syntax.type_value) =
  { name = "write_bytes_to_channel";
    identifiers =
      ["write_bytes_to_channel";
       datatype_name;
       "channel";
       "no_bytes"];
    type_scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Unit_Type),
                [Reference_Type (None, UserDefined_Type (None, -2));
                 Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Size_Type None)]);
    function_scheme = dud_function;
  }

let bytes_channel_to_stream (datatype_name : string) (ty : Crisp_syntax.type_value) =
  { name = "bytes_channel_to_stream";
    identifiers =
      ["bytes_channel_to_stream";
       "stream";
       "channel";
       "bytes_read";
       "bytes_written"];
    type_scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Unit_Type),
                [Reference_Type (Some (-2), Char_Type None);
                 Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Size_Type None);
                 Reference_Type (Some (-5), Size_Type None)]);
    function_scheme = dud_function;
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
