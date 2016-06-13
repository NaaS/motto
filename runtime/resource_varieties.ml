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
     end : REFERENCE_Instance)
end
