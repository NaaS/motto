(*
   Generation of de/serialisers from Flick types
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty
open Naasty_aux
open Translation
open State
open State_aux

(*Thrown when we try to generate a de/serialiser for a type that cannot be
  serialised -- either because of its nature (e.g., unit) or because it lacks
  annotations.*)
exception Unserialisable

type translated_type =
  { naasty_type : naasty_type;
    serialiser : naasty_function option;
    deserialiser : naasty_function option }

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
  (["get_channel_len"],
   Fun_Type (-1, Size_Type None, []))
let get_stream_len =
  (["get_stream_len"],
    Fun_Type (-1, Size_Type None, []))
let bytes_stream_to_channel datatype =
  (["bytes_stream_to_channel";
    datatype;
    "stream";
    "channel";
    "streamend";
    "bytes_read";
    "bytes_written"],
   Fun_Type (-1,
             Static_Type (None,
                          Reference_Type (None,
                                          UserDefined_Type (None, -2))),
             [Reference_Type (Some (-3), Char_Type None);
              Reference_Type (Some (-4), Char_Type None);
              Reference_Type (Some (-5), Char_Type None);
              Reference_Type (Some (-6), Size_Type None);
              Reference_Type (Some (-7), Size_Type None)]))
let write_bytes_to_channel datatype =
  (["write_bytes_to_channel";
    datatype;
    "channel";
    "no_bytes"],
   Fun_Type (-1,
             Static_Type (None,
                          Unit_Type),
             [Reference_Type (None, UserDefined_Type (None, -2));
              Reference_Type (Some (-3), Char_Type None);
              Reference_Type (Some (-4), Size_Type None)]))
let bytes_channel_to_stream =
  (["bytes_channel_to_stream";
    "stream";
    "channel";
    "bytes_read";
    "bytes_written"],
   Fun_Type (-1,
             Static_Type (None,
                          Unit_Type),
             [Reference_Type (Some (-2), Char_Type None);
              Reference_Type (Some (-3), Char_Type None);
              Reference_Type (Some (-4), Size_Type None);
              Reference_Type (Some (-5), Size_Type None)]))


(*
How this works:
- Given a Flick type we traverse it in the presented order (i.e., in the case of
   a record we proceed field by field in the given order)
- We look at each component of the type, starting with the leaves, and at the
   annotation of each component, to determine ????
- Variant's aren't supported: they're not used in our use-cases. Adding support
   for variants (at least for simple matching) isn't hard, it involves matching
   a token to determine which projection we're working in.
*)
let gen_serialiser (ty : type_value) : naasty_function =
  failwith "TODO"


let gen_deserialiser (ty : type_value) : naasty_function =
  failwith "TODO"

;;
(*FIXME crude test*)
fold_map ([], initial_state) (fun st scheme ->
      instantiate true (fst scheme) st (snd scheme))
  [get_channel_len; get_stream_len; bytes_stream_to_channel "test";
   write_bytes_to_channel "test"; bytes_channel_to_stream]
|> (fun (tys, st) ->
  let st_s = state_to_str false st in
  let res_s = List.map (string_of_naasty_type ~st_opt:(Some st) 0) tys
    |> String.concat ";\n" in
  st_s ^ res_s)
|> print_endline
