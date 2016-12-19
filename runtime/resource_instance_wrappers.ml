(*
   Wrappers for resources that can be used during runtime. The wrappers enable
   us to have various types of manifestations of a particular kind of resource
   without falling afoul of OCaml's type system.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open Resource_types

module type REFERENCE_Instance =
  sig
    (*The type of reference*)
    module Reference : REFERENCE
    (*Our representation of the resource's state*)
    val state : Reference.t
    (*General name for this kind of resource, used when
      printing diagnostic messages.*)
    val name : string
  end

module type DICTIONARY_Instance =
  sig
    module Dictionary : DICTIONARY
    val state : Dictionary.t
    val name : string
  end

module type CHANNEL_Instance =
  sig
    module Channel : CHANNEL
    val state : Channel.t
    val name : string
    (*The Flick-level name of the channel. This is used
      in the 'devaluate' function, to return a reference
      to the channel (via its name).
      FIXME is there a better way of keeping this name?
      NOTE we can alias external resources, but in this case
           varname would be shared by the aliased Flick identifiers.
           That is, it would map to one of them, and this might
           be misleading.
    *)
    val varname : string
  end
