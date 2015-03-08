(*
   Constants used in Data_model module.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

module type Datatype =
sig
  (*The Crisp type the definition of which we're expanding to use the data
    model.*)
  val ty : Crisp_syntax.type_value

  (*The name of the Crisp type 'ty' above.
    FIXME i think we could extract this directly from 'ty'.*)
  val datatype_name : string
end

module type Values =
sig
  (*The Crisp type the definition of which we're expanding to use the data
    model.*)
  val ty : Crisp_syntax.type_value

  (*String literals that can be used in a template: these need to be specified by us*)
  val get_channel_lenK : string
  val get_stream_lenK : string
  val bytes_stream_to_channelK : string
  val write_bytes_to_channelK : string
  val bytes_channel_to_streamK : string
  val streamK : string
  val channelK : string
  val streamendK : string
  val lenK : string
  val sizeofK : string
  val readWriteData_encodeVIntSizeK : string
  val bytes_readK : string
  val bytes_writtenK : string
  val no_bytesK : string
  val datatype_nameK : string
  val datatype_gclK : string
  val datatype_gslK : string
  val datatype_bstcK : string
  val datatype_wbtcK : string
  val datatype_bctsK : string

  (*This collects all the string literals described above*)
  val identifiers : string list

  (*Placeholders for the string literals -- these need to be declared, but their
  precise value will be generated automatically.*)
  val streamI : int
  val channelI : int
  val streamendI : int
  val get_channel_lenI : int
  val lenI : int
  val sizeofI : int
  val get_stream_lenI : int
  val readWriteData_encodeVIntSizeI : int
  val bytes_stream_to_channelI : int
  val bytes_readI : int
  val bytes_writtenI : int
  val write_bytes_to_channelI : int
  val no_bytesI : int
  val bytes_channel_to_streamI : int
  val datatype_nameI : int
  val datatype_gclI : int
  val datatype_gslI : int
  val datatype_bstcI : int
  val datatype_wbtcI : int
  val datatype_bctsI : int
end

module Values (Datatype : Datatype) : Values =
struct

let ty = Datatype.ty

let get_channel_lenK = "get_channel_len"
let get_stream_lenK = "get_stream_len"
let bytes_stream_to_channelK = "bytes_stream_to_channel"
let write_bytes_to_channelK = "write_bytes_to_channel"
let bytes_channel_to_streamK = "bytes_channel_to_stream"

let streamK = "stream"
let channelK = "channel"
let streamendK = "streamend"
let lenK = "len"
let sizeofK = "sizeof"
let readWriteData_encodeVIntSizeK = "ReadWriteData::encodeVIntSize"
let bytes_readK = "bytes_read"
let bytes_writtenK = "bytes_written"
let no_bytesK = "no_bytes"

(*The following aren't fixed -- they depend on the value of datatype_name*)
let datatype_nameK = Datatype.datatype_name
let datatype_gclK = Datatype.datatype_name ^ "::" ^ get_channel_lenK
let datatype_gslK = Datatype.datatype_name ^ "::" ^ get_stream_lenK
let datatype_bstcK = Datatype.datatype_name ^ "::" ^ bytes_stream_to_channelK
let datatype_wbtcK = Datatype.datatype_name ^ "::" ^ write_bytes_to_channelK
let datatype_bctsK = Datatype.datatype_name ^ "::" ^ bytes_channel_to_streamK

let identifiers =
  [
    streamK;
    channelK;
    streamendK;
    get_channel_lenK;
    lenK;
    sizeofK;
    get_stream_lenK;
    readWriteData_encodeVIntSizeK;
    bytes_stream_to_channelK;
    bytes_readK;
    bytes_writtenK;
    write_bytes_to_channelK;
    no_bytesK;
    bytes_channel_to_streamK;
    datatype_nameK;
    datatype_gclK;
    datatype_gslK;
    datatype_bstcK;
    datatype_wbtcK;
    datatype_bctsK;
  ]

let ident_placeholder s =
  let _, ans =
    List.fold_right (fun ident (idx, ans) ->
      match ans with
      | Some _ -> (idx, ans)
      | None ->
        if ident = s then (idx, Some idx)
        else (idx - 1, None))
      (List.rev identifiers) (-1, None)
  in match ans with
  | None ->
    failwith ("Could not calculate placeholder for the identifier " ^ s)
  | Some idx ->
    if idx < - List.length identifiers then
      failwith ("Could not calculate placeholder for the identifier " ^ s)
    else idx

let streamI,
    channelI,
    streamendI,
    get_channel_lenI,
    lenI,
    sizeofI,
    get_stream_lenI,
    readWriteData_encodeVIntSizeI,
    bytes_stream_to_channelI,
    bytes_readI,
    bytes_writtenI,
    write_bytes_to_channelI,
    no_bytesI,
    bytes_channel_to_streamI,
    datatype_nameI,
    datatype_gclI,
    datatype_gslI,
    datatype_bstcI,
    datatype_wbtcI,
    datatype_bctsI
  =
  ident_placeholder streamK,
  ident_placeholder channelK,
  ident_placeholder streamendK,
  ident_placeholder get_channel_lenK,
  ident_placeholder lenK,
  ident_placeholder sizeofK,
  ident_placeholder get_stream_lenK,
  ident_placeholder readWriteData_encodeVIntSizeK,
  ident_placeholder bytes_stream_to_channelK,
  ident_placeholder bytes_readK,
  ident_placeholder bytes_writtenK,
  ident_placeholder write_bytes_to_channelK,
  ident_placeholder no_bytesK,
  ident_placeholder bytes_channel_to_streamK,
  ident_placeholder datatype_nameK,
  ident_placeholder datatype_gclK,
  ident_placeholder datatype_gslK,
  ident_placeholder datatype_bstcK,
  ident_placeholder datatype_wbtcK,
  ident_placeholder datatype_bctsK
end
