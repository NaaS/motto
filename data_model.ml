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
    function_scheme : Crisp_syntax.type_value -> naasty_function }

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
let get_channel_len (datatype_name : string) =
  let name = "get_channel_len" in
  { name = name;
    identifiers = [name];
    type_scheme = Fun_Type (-1, Size_Type None, []);
    function_scheme =
      fun (ty : Crisp_syntax.type_value) ->
        let fun_name_idx = -1 in
        let arg_tys = [] in
        let ret_ty = Size_Type None in
        let body =
          [
            Declaration (Size_Type (Some (-2)));
            Assign (-2, Int_Value 0);
            (*FIXME fill in the rest of the body*)
            Return (Var (-2))
          ] |> Naasty_aux.concat
        in (fun_name_idx, arg_tys, ret_ty, body);
  }

let get_stream_len (datatype_name : string) =
  { name = "get_stream_len";
    identifiers = ["get_stream_len"];
    type_scheme = Fun_Type (-1, Size_Type None, []);
    function_scheme = fun _ -> failwith "TODO";
  }

let bytes_stream_to_channel (datatype_name : string) =
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
    function_scheme = fun _ -> failwith "TODO";
  }

let write_bytes_to_channel (datatype_name : string) =
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
    function_scheme = fun _ -> failwith "TODO";
  }

let bytes_channel_to_stream (datatype_name : string) =
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
    function_scheme = fun _ -> failwith "TODO";
  }

(*Instantiates the data model for a particular serialisable datatype.
  This should generate part of the "struct" definition in the resulting C++
  translation of the Flick type. *)
let instantiate_data_model (datatype_name : string) =
  [get_channel_len datatype_name;
   get_stream_len datatype_name;
   bytes_stream_to_channel datatype_name;
   write_bytes_to_channel datatype_name;
   bytes_channel_to_stream datatype_name]
