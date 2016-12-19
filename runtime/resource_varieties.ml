(*
   Catalogue of the variety of different kinds of resources we support.
   Where "kind of resource" is "channel", "dictionary", etc
   and "variety of (channel) resource" is "FIFO", "socket", etc.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open Resource_types
open Resource_instance_wrappers

(*Collects the varieties of references we support*)
module Reference =
struct
  let local_reference n =
    (module struct
       module Reference = Resource_instances.Reference
       let state = Reference.allocate (Some n)
       let name = "local_reference"
     end : REFERENCE_Instance)
end

module Dictionary =
struct
  let local_hashtable n =
    (module struct
       module Dictionary = Resource_instances.Dictionary
       let state = Resource_instances.Dictionary.allocate (Some 10)
       let name = "local_hashtable"
     end : DICTIONARY_Instance)
end

module Channel =
struct
  let fifo varname (*FIXME assign some sort of resource quantity? for size of buffers, for instance*) =
    (module struct
       module Channel = Resource_instances.Channel_FIFO
       let state = Channel.allocate None
       let name = "fifo"
       let varname = varname
     end : CHANNEL_Instance)
end
