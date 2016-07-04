(*
   Interface between the runtime and bytes. This interface consists of buffers
   and parsers. These mediate with resources, which are the source or destination
   of bytes.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


open Crisp_syntax
open Resource_types

module Buffer : BUFFER =
struct
type t =
  {
    buffer_size : int;
    buffer : Bytes.t;
    (*write_ofs is expected to be ahead or equal
      to read_ofs, to implement a ring buffer.*)
    write_ofs : int ref;
    read_ofs : int ref;
    filler : (bytes -> int -> int -> int) ref;
  }

let create ?init_value:(init_value = 0x0) size =
  {
    buffer_size = size;
    buffer = Bytes.make size (char_of_int init_value);
    write_ofs = ref 0;
    read_ofs = ref 0;
    filler = ref (fun _ ->
      (*FIXME improve error -- have custom exception?*)
      failwith "Filler not registered with buffer")
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

let register_filler t f = t.filler := f

(*This function attempts to fill "no_bytes_needed" bytes from the resource
  into the buffer. Should fewer than "no_bytes_needed" be currently available
  from the resource, then all of them are filled up. The function returns the
  number of bytes that it has filled.*)
let fill t no_bytes_needed =
  let no_bytes_read = !(t.filler) t.buffer !(t.write_ofs) no_bytes_needed in
  (*NOTE the ring buffer's write pointer is encapsulated
         from the filler, and it's up to the buffer to update it.
         This is done next.*)
  t.write_ofs := no_bytes_read + !(t.write_ofs);
  no_bytes_read

let fill_until t n =
  (*Test the filler -- have it read 0 bytes. This should result in an exception
    if a filler hasn't been registered.*)
  ignore(!(t.filler) t.buffer 0 0);
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
     (*Split into two cases, in case there's wrapping-around*)
     let no_bytes_read =
       if no_bytes_needed + !(t.write_ofs) < size t then
         fill t no_bytes_needed
       else
         let part1_width = size t - !(t.write_ofs) in
         let part2_width = no_bytes_needed - part1_width in
         let part1_nobytesread = fill t part1_width in
         let part2_nobytesread =
           (*If fewer than part1_width bytes were filled
             so far, then don't bother trying to fill more since
             the resource obviously doesn't have them.*)
           if part1_nobytesread = part1_width then
             fill t part2_width
           else 0 in
         part1_nobytesread + part2_nobytesread in
     no_bytes_read = no_bytes_needed)

let read t n =
  assert (n > 0);
  assert (!(t.read_ofs) <> !(t.write_ofs));
  if n > occupied_size t then
    None
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
      Some result
    end

(*Lifts a write-related function to work modulo the size of a buffer (and taking
  into account the read_ofs). It's also given a paired function that's called
  when the lifting cannot succeed. This occurs when the write_ofs would overlap
  with read_ofs.
  "lift_mod" raises exceptions when the offset it's given is obviously wrong --
  way out of range.*)
let lift_mod t offset
 (can : t -> int(*offset*) -> 'a)
 (cannot : t -> int(*offset*) -> 'a) : 'a =
  assert (offset >= 0);
  (*Check that the requested offset doesn't overlap with bytes in read_ofs*)
  if !(t.write_ofs) = !(t.read_ofs) then
    (*We can read any byte, since none's there for reading.*)
    can t offset
  else if !(t.write_ofs) < !(t.read_ofs) then
    (*write_ofs shouldn't extend beyond read_ofs*)
    if !(t.write_ofs) + offset < !(t.read_ofs) then
      can t offset
    else
      (*We don't raise an exception in this case since the problem seems
        tolerable: the user is not doing something very wrong, such as trying to
        write more data than the whole buffer can take.*)
      cannot t offset
  else
    begin
    assert (!(t.write_ofs) > !(t.read_ofs));
    if offset >= size t then
      (*if wrap more than once, then we have a fail*)
      (*FIXME give more info*)
      failwith "The offset exceeds the buffer's size"
    else
      if !(t.write_ofs) + offset < size t then
        (*if wrap zero times, then write_ofs must stay bigger (modulo size) then read_ofs*)
        if !(t.write_ofs) + offset > !(t.read_ofs) then
          can t offset
        else
          (*We don't raise an exception in this case since the problem seems
            tolerable: the user is not doing something very wrong, such as
            trying to write more data than the whole buffer can take.*)
          cannot t offset
      else if !(t.write_ofs) + offset mod size t >= !(t.read_ofs) then
        (*if we wrap once (the only option left), then write_ofs must be
          smaller (modulo size) than read_ofs*)
        cannot t offset
     else can t offset
    end

let read_byte_mod t offset : char option =
  let can t offset : char option =
    Some (Bytes.get t.buffer ((!(t.write_ofs) + offset) mod size t)) in
  let cannot _ _ : char option = None in
  lift_mod t offset can cannot

let write_byte_mod t offset byte : bool =
  let can t offset : bool =
    Bytes.set t.buffer ((!(t.write_ofs) + offset) mod size t) byte;
    true in
  let cannot _ _ : bool = false in
  lift_mod t offset can cannot

let effect_write_byte_mod t length : bool =
  let can t offset : bool =
    t.write_ofs := (!(t.write_ofs) + offset) mod size t;
    true in
  let cannot _ _ : bool = false in
  lift_mod t length can cannot
end

let _ZERO =
  (*FIXME is this const already defined in std libraries?*)
  char_of_int 0x0;;

(*Character-encoded decimals*)
module Decimal_Digit_Parser : PARSER_BUILDER = functor (Buffer : BUFFER) ->
struct
  type buffer = Buffer.t
  type t =
    {
      buffer : buffer
    }
  let init buf : t = { buffer = buf }
  let buffer t : buffer = t.buffer
  (*Bytes needed in a buffer in order to be able to
    parse an entity.
    Also, space needed in a buffer in order to unparse
    an expression into it.*)
  let space_needed = 1
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
        if Buffer.fill_until t.buffer space_needed(*need a single byte in the buffer to proceed*) then
          match Buffer.read t.buffer space_needed(*read that byte we have*) with
          | None -> Unavailable
          | Some bytes ->
            let c =
              (*Read the first and only byte*)
              Bytes.get bytes 0 in
            if c = _ZERO then Unavailable
            else
              Expression (Int (int_of_string (Char.escaped c)))
        else Unavailable
    | _ ->
      (*FIXME give more info*)
      failwith "Type not supported by parser"

  let unparse t tv e : bool =
    match tv with
    | Integer _(*FIXME should we differentiate based
                       on this parameter?*) ->
      (*We're being asked to emit an integer to
        the buffer referenced by "t".*)
      begin
      match e with
      | Int n ->
        assert (0 <= n && n <= 9);
        (*Ensure there's enough free space in the buffer so we can write to it.*)
        if Buffer.size t.buffer - Buffer.occupied_size t.buffer < space_needed then false
        else
          let s = string_of_int n in
          if Buffer.write_byte_mod t.buffer 0 s.[0] then
            Buffer.effect_write_byte_mod t.buffer space_needed
          else false
      end
    | _ ->
      (*FIXME give more info*)
      failwith "Type not supported by parser"
end
