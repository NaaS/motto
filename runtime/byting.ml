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
    (*We must have wrapped around, evidence by the
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
  if (n > size t) then
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
     !(t.reader) no_bytes_needed = no_bytes_needed)
end
