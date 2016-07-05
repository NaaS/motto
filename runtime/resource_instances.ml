(*
   Resources that can be used during runtime, to store data and interact with
   the environment.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


open Crisp_syntax
open Resource_types
open Byting

(*Instead of un/parsing expressions, we store them directly in this example.*)
(*FIXME implement example involving remote reference?*)
module Reference : REFERENCE =
struct
  type t = bool ref * expression ref

  let allocate (Some n) =
    (*FIXME we currently allow storing expressions of any size in the reference!*)
    assert (n = 1);
    (ref false, ref Bottom)

  let initialise (_r_assigned, _r) s_opt =
    assert (not !_r_assigned);
    match s_opt with
    | None -> failwith "Initial value required"
    | Some e_s ->
      begin
        let e =
          match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
          | Expression e -> e
          | _ ->
            failwith ("Could not parse into an expression: " ^ e_s) in
        _r := e;
      end;
    _r_assigned := true;
    !_r_assigned

  let is_available (_r_assigned, _) =
    !_r_assigned

  let update (_r_assigned, _r) e =
    assert !_r_assigned;
    _r := e;
    Expression e

  let retrieve (_r_assigned, _r) =
    assert !_r_assigned;
    Expression !_r

  let dismiss (_r_assigned, _) =
    assert !_r_assigned;
    _r_assigned := false;
    not (!_r_assigned)
end

module Dictionary : DICTIONARY =
struct
  type t = {
    availability : bool ref;
    capacity : int;
    (*NOTE we'd prefer to use 'typed_value' not 'expression' here, but with the
           current code it creates a circular dependency among modules. In the
           reference above we stored 'expression'. The choice is a bit
           arbitrary, but using typed_value adds extra protection to ensure that
           we only store stuff that's "storable" in Flick.*)
    content : (expression, expression) Hashtbl.t;
  }

  let allocate (Some n) = {
    availability = ref true;
    capacity = n;
    content = Hashtbl.create ~random:false n;
  }

  let initialise t s_opt =
    (*We initialised the dictionary durine 'allocate' above*)
    assert (s_opt = None);
    !(t.availability)

  let is_available t = !(t.availability)

  let dismiss t =
    t.availability := false;
    not (!(t.availability))

  let lookup t k_e =
    assert !(t.availability);
    Expression (Hashtbl.find t.content k_e)

  let update t k_e v_e =
    assert !(t.availability);
    assert (Hashtbl.length t.content < t.capacity);
    Hashtbl.replace t.content k_e v_e;
    Expression v_e

  let delete t k_e =
    assert !(t.availability);
    Hashtbl.remove t.content k_e;
    Expression Crisp_syntax.flick_unit_value

  let key_exists t k_e =
    assert !(t.availability);
    let result =
      Hashtbl.mem t.content k_e
      |> Crisp_syntax_aux.lift_bool in
    Expression result

  let size t =
    assert !(t.availability);
    Expression (Int (Hashtbl.length t.content))

  let capacity t =
    assert !(t.availability);
    Expression (Int (t.capacity))

  let as_expression t =
    failwith "TODO"
end

(*FIXME could instantiate DICTIONARY to give Flick programs access to the
        process environment, though that might incline them too much in the
        direction of Unix*)
let _BUFSIZE (*FIXME const*) = 200;;
let _ZERO =
  (*FIXME is this const already defined in std libraries?*)
  char_of_int 0x0;;

module Channel_FIFO_old : CHANNEL_old =
(*FIXME make functor to accept parser, which decides on its buffering,
        and returns an expression*)
struct
  type t = {
    target : string ref;
    fd : Unix.file_descr option ref;

    (*Callback expressions. These must be of type "unit"*)
    on_attach : expression ref;
    on_detach : expression ref;

    (*RX and TX buffers. These are backed by a ring buffer of bytes.
      In RX, the ring buffer feeds a buffer of parsed expressions*)
    rx_pre_parse_buffer : bytes;
    rx_write_ofs : int ref;
    rx_read_ofs : int ref;
    rx_buffer : expression list ref;
    (*In TX the ring buffer feeds from a buffer of parsed expressions*)
    tx_post_unparse_buffer : bytes;
    tx_write_ofs : int ref;
    tx_read_ofs : int ref;
    tx_buffer : expression list ref;
  }

  let allocate n_opt =
    assert (n_opt = None);
    {
      target = ref "";
      fd = ref None;
      on_attach = ref flick_unit_value;
      on_detach = ref flick_unit_value;
      rx_buffer = ref [];
      tx_buffer = ref [];

      (*Allocate the buffers an initialise them to contain the character '0'*)
      rx_pre_parse_buffer = Bytes.make _BUFSIZE _ZERO;
      tx_post_unparse_buffer = Bytes.make _BUFSIZE _ZERO;
      rx_write_ofs = ref 0;
      rx_read_ofs = ref 0;
      tx_write_ofs = ref 0;
      tx_read_ofs = ref 0;
     }

  let initialise t (Some s) =
    let flags =
      [Unix.O_NOCTTY; (*Because we don't expect to interact with a TTY via this channel.*)
       Unix.O_CLOEXEC; (*For defensive programming; we don't expect to spawn processes anyway.*)
       Unix.O_NONBLOCK; (*We don't want blocking, so our runtime can retain more control over interaction with resources.*)
      (*NOTE we set access to read&write irrespective of the channel's type,
             since read&write fifo gives us more control over its use. For example,
             no EOF will be received if we're write-only and the other (reading)
             side has closed its side of the fifo.
             We'll have to be careful to respect the channel's type, not to write
             on a ready-only channel for instance.*)
       Unix.O_RDWR;
      ] in
    t.target := s;
    (*t.fd := Some (Unix.openfile s flags 0o600);*)
    t.fd := Some (Unix.openfile s flags 0);
    (*FIXME check status, and return 'false' if something's wrong*)
    true

  let is_available t = (!(t.fd) <> None)

  let dismiss t =
    assert (!(t.fd) <> None);
    General.the (!(t.fd))
    |> Unix.close;
    t.fd := None;
    t.target := "";
    t.on_attach := flick_unit_value;
    t.on_detach := flick_unit_value;
    t.rx_buffer := [];
    (*FIXME we should flush the tx_buffer first, rather than simply ignore its contents*)
    t.tx_buffer := [];
    (*FIXME zero-out the byte buffers, and reset the read/write offset "pointers"*)
    true

  let attach_to t e =
    (*FIXME could carry out the file opening here instead of in "initialise".
            this also has the advantage that t.on_attach and t.on_detach could
            be defined by the programmer at this point, while at "initialise"
            they'd still have their initial value.*)
    failwith "TODO"

  let attached_to t =
    assert (!(t.fd) <> None);
    Expression (Str !(t.target))

  let on_attachment t e =
    assert (!(t.fd) <> None);
(*  FIXME ensure that 'e' is typed 'unit'
    let ty, _ = Type_infer.ty_of_expr st e in
    assert (ty = ...);
*)
    t.on_attach := e

  let on_detachment t e =
    assert (!(t.fd) <> None);
(*  FIXME ensure that 'e' is typed 'unit'
    let ty, _ = Type_infer.ty_of_expr st e in
    assert (ty = ...);
*)
    t.on_detach := e

  (*Read from the fd until the rx_buffer is at maximum capacity (if it has a
    maximum capacity), or until the fd has no more to give at this time.*)
  (*FIXME we disregard maximum capacity at the moment*)
  let read_AMAP t : unit =
    assert (!(t.fd) <> None);

    let parse (s : string) : expression =
print_endline ("parsed:" ^ s);
      (*FIXME really basic! only works for integers.*)
      Int (int_of_string s) in

    let fd = General.the !(t.fd) in
    let bufsize = Bytes.length t.rx_pre_parse_buffer in
    let quant =
      if !(t.rx_read_ofs) > !(t.rx_write_ofs) then
        (*Can write until just short of the read offset, otherwise we'd
          overwrite stuff that hasn't yet been parsed.*)
        !(t.rx_read_ofs) - !(t.rx_write_ofs)
      else
        (*Can write until the end of the buffer*)
        bufsize - !(t.rx_write_ofs) in
print_endline ("quant:" ^ string_of_int quant);
print_endline ("rx_write_ofs:" ^ string_of_int !(t.rx_write_ofs));

    (*Read AMAP into rx_pre_parse_buffer*)
    let written_quant =
      try
        Unix.read fd t.rx_pre_parse_buffer
         !(t.rx_write_ofs) quant
      (*Update ring buffer's write pointer*)
      with
      (*Since we're in non-blocking mode, ignore such exceptions;
        interpret them to mean that no data is currently available.*)
      | Unix.Unix_error (Unix.EAGAIN, "read", _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, "read", _) -> 0 in
    t.rx_write_ofs := (!(t.rx_write_ofs) + written_quant) mod bufsize;

print_endline ("written_quant:" ^ string_of_int written_quant);
print_endline ("rx_write_ofs:" ^ string_of_int !(t.rx_write_ofs));
    (*Parse AMAP from rx_pre_parse_buffer into rx_buffer*)
    (*Look for string terminator, possibly wrapping around the buffer
      FIXME actually we just treat each byte as an int at the moment.*)
    let stop = ref false in

    (*FIXME inefficient! we do this because we'll be adding stuff at the end*)
    t.rx_buffer := List.rev !(t.rx_buffer);

print_endline ("|rx_buffer|:" ^ string_of_int (List.length !(t.rx_buffer)));
    for i = !(t.rx_read_ofs) to
            !(t.rx_read_ofs) + bufsize (*we will use mod to wrap*)
    do
      (*FIXME find way to break out of the loop if we've reached rx_write_ofs*)
      if !stop || i = !(t.rx_write_ofs) then
        stop := true
      else
      begin
        let c = Bytes.get t.rx_pre_parse_buffer (i mod bufsize) in
        if c = _ZERO then
          stop := true
        else
        begin
          t.rx_buffer := Str (Char.escaped c) :: !(t.rx_buffer);
          (*Update ring buffer's read pointer*)
          t.rx_read_ofs := 1 + !(t.rx_read_ofs);
        end
      end
    done;
    (*FIXME inefficient! we do this to restore the order in which values arrived*)
print_endline ("|rx_buffer|:" ^ string_of_int (List.length !(t.rx_buffer)));
    t.rx_buffer := List.rev !(t.rx_buffer)

  (*Write to the fd until the tx_buffer is empty, or until the fd cannot accept
    any more at this time.*)
  let write_AMAP t : unit =
    assert (!(t.fd) <> None);
    failwith "TODO"

  let can_receive t =
    assert (!(t.fd) <> None);
    (*FIXME check type of channel -- can we receive on it?*)
    (*FIXME check waiting contents of channel -- is there anything waiting to be read?*)
    read_AMAP t;
    Expression
     ((!(t.rx_buffer) <> [])
      |> Crisp_syntax_aux.lift_bool)

  let size_receive t =
    assert (!(t.fd) <> None);
    (*FIXME check type of channel -- if we cannot receive on it, then "size" is -1*)
    (*FIXME check waiting contents of channel -- is there anything waiting to be read?*)
    read_AMAP t;
    Expression (Int (List.length !(t.rx_buffer)))

  let can_send t =
    assert (!(t.fd) <> None);
    failwith "TODO"
  let size_send t =
    assert (!(t.fd) <> None);
    failwith "TODO"

  let peek t =
    assert (!(t.fd) <> None);
    read_AMAP t;
    let result =
      match !(t.rx_buffer) with
      | [] -> Unavailable
      | e :: _ ->
        Expression e
    in result

  let receive t =
    assert (!(t.fd) <> None);
    read_AMAP t;
    let result =
      match !(t.rx_buffer) with
      | [] -> Unavailable
      | e :: es ->
        t.rx_buffer := es;
        Expression e
    in result

  let send t e =
    assert (!(t.fd) <> None);
    failwith "TODO"
end

(*Channel accepts a parser, which decides on its buffering,
  and returns an expression when one's received on the channel.*)
module Channel_FIFO_Builder : CHANNEL_BUILDER = functor (Parser_Fun : PARSER_BUILDER) (RX_Buffer : BUFFER) ->
struct
  module Parser = Parser_Fun (RX_Buffer)

  type t = {
    target : string ref;
    fd : Unix.file_descr option ref;

    (*Callback expressions. These must be of type "unit"*)
    on_attach : expression ref;
    on_detach : expression ref;

    buffr : RX_Buffer.t;
    parsr : Parser.t;
  }

  let allocate n_opt =
    assert (n_opt = None);
    let buffr = RX_Buffer.create 200(*FIXME const*) in
    {
      target = ref "";
      fd = ref None;
      on_attach = ref flick_unit_value;
      on_detach = ref flick_unit_value;
      buffr = buffr;
      parsr = Parser.init buffr;
    }

  let initialise t (Some s) =
    let flags =
      [Unix.O_NOCTTY; (*Because we don't expect to interact with a TTY via this channel.*)
       Unix.O_CLOEXEC; (*For defensive programming; we don't expect to spawn processes anyway.*)
       Unix.O_NONBLOCK; (*We don't want blocking, so our runtime can retain more control over interaction with resources.*)
      (*NOTE we set access to read&write irrespective of the channel's type,
             since read&write fifo gives us more control over its use. For example,
             no EOF will be received if we're write-only and the other (reading)
             side has closed its side of the fifo.
             We'll have to be careful to respect the channel's type, not to write
             on a ready-only channel for instance.*)
       Unix.O_RDWR;
      ] in
    t.target := s;
    (*NOTE "mode" must be 0 since we're not (expecting to be) creating the file.*)
    let mode = 0 in
    let fd = Unix.openfile s flags mode in
    t.fd := Some fd;
    (fun raw_buffer offset qty ->
     (*FIXME check that qty <= buffer size.*)
     try
       Unix.read fd raw_buffer offset qty
       (*NOTE the ring buffer's write pointer is encapsulated
              from the filler, and it's up to the buffer to update it.*)
     with
     (*Since we're in non-blocking mode, ignore such exceptions;
       interpret them to mean that no data is currently available.*)
     | Unix.Unix_error (Unix.EAGAIN, "read", _)
     | Unix.Unix_error (Unix.EWOULDBLOCK, "read", _) -> 0)
    |> RX_Buffer.register_filler t.buffr;

    (fun raw_buffer offset qty ->
     (*FIXME check that qty <= buffer size.*)
       Unix.write fd raw_buffer offset qty
       (*NOTE the ring buffer's write pointer is encapsulated
              from the filler, and it's up to the buffer to update it.*)
)
    |> RX_Buffer.register_unfiller t.buffr; (*FIXME should be in TX_Buffer?*)

    (*FIXME check status, and return 'false' if something's wrong*)
    true

  let is_available t = (!(t.fd) <> None)

  let dismiss t =
    assert (!(t.fd) <> None);
    General.the (!(t.fd))
    |> Unix.close;
    t.fd := None;
    t.target := "";
    t.on_attach := flick_unit_value;
    t.on_detach := flick_unit_value;
    true

  let attach_to t e =
    (*FIXME could carry out the file opening here instead of in "initialise".
            this also has the advantage that t.on_attach and t.on_detach could
            be defined by the programmer at this point, while at "initialise"
            they'd still have their initial value.*)
    failwith "TODO"

  let attached_to t =
    assert (!(t.fd) <> None);
    Expression (Str !(t.target))

  let on_attachment t e =
    assert (!(t.fd) <> None);
(*  FIXME ensure that 'e' is typed 'unit'
    let ty, _ = Type_infer.ty_of_expr st e in
    assert (ty = ...);
*)
    t.on_attach := e

  let on_detachment t e =
    assert (!(t.fd) <> None);
(*  FIXME ensure that 'e' is typed 'unit'
    let ty, _ = Type_infer.ty_of_expr st e in
    assert (ty = ...);
*)
    t.on_detach := e

  let can_receive t =
    assert (!(t.fd) <> None);
    RX_Buffer.fill_until t.buffr (RX_Buffer.size t.buffr);
print_endline ("|rx_buffer|:" ^ string_of_int (RX_Buffer.size t.buffr));
print_endline ("!rx_buffer!:" ^ string_of_int (RX_Buffer.occupied_size t.buffr));

    (*FIXME check type of channel -- can we receive on it?*)
    (*FIXME check waiting contents of channel -- is there anything waiting to be read?*)
    Expression
     ((*FIXME there might not be sufficient data
              for the parser to operate on, so we
              should instead query the parser about this*)
      RX_Buffer.occupied_size t.buffr > 0
      |> Crisp_syntax_aux.lift_bool)

  let size_receive t =
    assert (!(t.fd) <> None);
    RX_Buffer.fill_until t.buffr (RX_Buffer.size t.buffr);
print_endline ("|rx_buffer|:" ^ string_of_int (RX_Buffer.size t.buffr));
print_endline ("!rx_buffer!:" ^ string_of_int (RX_Buffer.occupied_size t.buffr));
    (*FIXME check type of channel -- if we cannot receive on it, then "size" is -1*)
    (*FIXME check waiting contents of channel -- is there anything waiting to be read?*)
    let size =
      (*FIXME there might not be sufficient data
              for the parser to operate on, so we
              should instead query the parser about this*)
      RX_Buffer.occupied_size t.buffr in
    Expression (Int size)

  let can_send t =
    assert (!(t.fd) <> None);
    failwith "TODO"
  let size_send t =
    assert (!(t.fd) <> None);
    failwith "TODO"

  let peek t =
    assert (!(t.fd) <> None);
    (*FIXME BUFFER needs to support peeking*)
    failwith "TODO"

  let receive t =
    assert (!(t.fd) <> None);
    Parser.parse t.parsr
     (Integer (None, Crisp_type_annotation.empty_type_annotation))

  let send t e =
    assert (!(t.fd) <> None);
    (*FIXME improve code style*)
    let result =
    if Parser.unparse t.parsr
       (Integer (None, Crisp_type_annotation.empty_type_annotation)) e then
    Expression e else Unavailable in
print_endline ("|tx_buffer|:" ^ string_of_int (RX_Buffer.size t.buffr));
print_endline ("!tx_buffer!:" ^ string_of_int (RX_Buffer.occupied_size t.buffr));
    (*FIXME rather than doing this here, we could have another entity in the eval-monad
            that takes care of doing some IO whenever it's scheduled.*)
    RX_Buffer.unfill_until t.buffr (RX_Buffer.size t.buffr);
print_endline ("|tx_buffer|:" ^ string_of_int (RX_Buffer.size t.buffr));
print_endline ("!tx_buffer!:" ^ string_of_int (RX_Buffer.occupied_size t.buffr));
    result
end

module Channel_FIFO = Channel_FIFO_Builder (Decimal_Digit_Parser) (Buffer);;

(*
(*This is an example of using channels as an interface for asynchronous
  event processing or functions. This can also be used for non-blocking
  read and write to a device, for instance: we send the request on a channel,
  and set up a listener for a reply on that channel.*)
module Channel_Timer : CHANNEL =
struct
  ...
end

module Channel_Socket : CHANNEL =
struct
  ...
end
*)

(*FIXME could have console and error channels as instances of CHANNEL*)
