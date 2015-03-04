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
    scheme : naasty_type }

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
let get_channel_len =
  { name = "get_channel_len";
    identifiers = ["get_channel_len"];
    scheme = Fun_Type (-1, Size_Type None, [])}
let get_stream_len =
  { name = "get_stream_len";
    identifiers = ["get_stream_len"];
    scheme = Fun_Type (-1, Size_Type None, [])}
let bytes_stream_to_channel datatype =
  { name = "bytes_stream_to_channel";
    identifiers =
      ["bytes_stream_to_channel";
       datatype;
       "stream";
       "channel";
       "streamend";
       "bytes_read";
       "bytes_written"];
    scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Reference_Type (None,
                                             UserDefined_Type (None, -2))),
                [Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Char_Type None);
                 Reference_Type (Some (-5), Char_Type None);
                 Reference_Type (Some (-6), Size_Type None);
                 Reference_Type (Some (-7), Size_Type None)])}
let write_bytes_to_channel datatype =
  { name = "write_bytes_to_channel";
    identifiers =
      ["write_bytes_to_channel";
       datatype;
       "channel";
       "no_bytes"];
    scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Unit_Type),
                [Reference_Type (None, UserDefined_Type (None, -2));
                 Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Size_Type None)])}
let bytes_channel_to_stream =
  { name = "bytes_channel_to_stream";
    identifiers =
      ["bytes_channel_to_stream";
       "stream";
       "channel";
       "bytes_read";
       "bytes_written"];
    scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Unit_Type),
                [Reference_Type (Some (-2), Char_Type None);
                 Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Size_Type None);
                 Reference_Type (Some (-5), Size_Type None)])}

(*Instantiates the data model for a particular serialisable datatype.
  This should generate part of the "struct" definition in the resulting C++
  translation of the Flick type. *)
let instantiate_data_model datatype =
  [get_channel_len; get_stream_len; bytes_stream_to_channel datatype;
   write_bytes_to_channel datatype; bytes_channel_to_stream]
