(*
   Interface between the runtime and bytes. This interface consists of buffers
   and parsers. These mediate with resources, which are the source or destination
   of bytes.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


open Crisp_syntax
open Resource_types

module Buffer (*FIXME sig buffer_size : int end*) : BUFFER =
struct
type t =
  {
    buffer_size : int;
    buffer : Bytes.t;
    (*write_ofs is expected to be ahead or equal
      to read_ofs, to implement a ring buffer.*)
    write_ofs : int ref;
    read_ofs : int ref;
    reader : (int -> int) ref;
  }

let create ?init_value:(init_value = 0x0) size =
  {
    buffer_size = size;
    buffer = Bytes.make size (char_of_int init_value);
    write_ofs = ref 0;
    read_ofs = ref 0;
    reader = ref (fun _ ->
      (*FIXME improve error -- have custom exception?*)
      failwith "Reader not registered with buffer")
  }

let size t = t.buffer_size

(*occupied_size consits of the unread portion of the
  buffer -- i.e., write_ofs - read_ofs (modulo).
*)
let occupied_size t =
  if !(t.read_ofs) = !(t.write_ofs) then 0
  else if !(t.read_ofs) < !(t.write_ofs) then
    !(t.write_ofs) - !(t.read_ofs)
  else
    begin
    (*We must have wrapped around, evidenced by the
      next assertion holding*)
    assert (!(t.read_ofs) > !(t.write_ofs));
    (*So we can read from read_ofs until the end of
      the buffer, and from the beginning until write_ofs*)
    size t - !(t.read_ofs) + !(t.write_ofs)
    end

let register_reader t f = t.reader := f

let fill_until t n =
  (*Test the reader -- have it read 0 bytes. This should result in an exception
    if a reader hasn't been registered.*)
  ignore(!(t.reader) 0);
  if n > size t then
    (*FIXME give more info*)
    failwith "Read request exceeds buffer size"
  else
    (*We're done if we've got at least n bytes in
      the buffer*)
    (n <= occupied_size t ||
     (*Try to read as many bytes as we need to get
       the desired occupied_size.
       FIXME in case the granularity of the reads
             is too fine, we could additionally
             specify that a read tries to read a
             minimum number of bytes at a time,
             which it defaults to if n < minimum.*)
     let no_bytes_needed = n - occupied_size t in
     let no_bytes_read = !(t.reader) no_bytes_needed in
     t.write_ofs := no_bytes_read + !(t.write_ofs);
     no_bytes_read = no_bytes_needed)

let read t n =
  assert (n > 0);
  assert (!(t.read_ofs) <> !(t.write_ofs));
  if n > occupied_size t then
    (*FIXME give more info*)
    failwith "Read request exceeds occupied size"
  else
    let result =
      if !(t.read_ofs) < !(t.write_ofs) then
        !(t.write_ofs) - !(t.read_ofs)
        |> Bytes.sub t.buffer !(t.read_ofs)
      else
        begin
        assert (!(t.read_ofs) > !(t.write_ofs));
        Bytes.cat
          (size t - !(t.read_ofs)
           |> Bytes.sub t.buffer !(t.read_ofs))
          (Bytes.sub t.buffer 0 !(t.write_ofs))
        end in
    begin
      t.read_ofs := (!(t.read_ofs) + n) mod size t;
      result
    end

let raw t =
  (*FIXME ensure we don't get a copy of the buffer.*)
  t.buffer
end

let _ZERO =
  (*FIXME is this const already defined in std libraries?*)
  char_of_int 0x0;;

(*FIXME this doesn't really yet parse integers?*)
module Integer_Parser : PARSER = functor (Buffer : BUFFER) ->
struct
  type buffer = Buffer.t
  type t =
    {
      buffer : buffer
    }
  let init buf : t = { buffer = buf }
  let buffer t : buffer = t.buffer
  let parse t tv : result =
    match tv with
    | Integer _(*FIXME should we differentiate based
                       on this parameter?*) ->
      (*We're being asked to parse an integer from
        the buffer referenced by "t".*)
      (*Look into the buffer. If there's something
        we can parse (fully or partly) then do this.
        If we can parse it fully, then return it.
        Otherwise return Unavailable, and store
        intermediate state (partial parse), so we
        can resume when more data's in the buffer.
        Also, try to get more data in the buffer,
        so when the parser's scheduled next time
        it can continue with the job.*)
      (*NOTE rather than taking action to fill the
             buffer and parse when we're polled,
             we could instead have background threads
             that pull from the resource into the buffer,
             and other threads that pull from the buffer
             and parse the data (possibly storing the data
             into a second buffer), so when the parsed
             data's needed it can be retrieved directly,
             rather than invoke a series of actions
             that involve an IO step each time.*)
        if Buffer.fill_until t.buffer 1 then
          let c = (*FIXME bad code style*)
            Bytes.get (Buffer.read t.buffer 1) 0 in
          if c = _ZERO then Unavailable
          else
            (*FIXME note that this returns a string not an integer!*)
            Expression (Str (Char.escaped c))
        else Unavailable
    | _ ->
      (*FIXME give more info*)
      failwith "Type not supported by parser"
  let unparse t tv e : bool =
    failwith "TODO"
end
