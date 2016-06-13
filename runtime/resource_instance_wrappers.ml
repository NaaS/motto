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
    module Reference : REFERENCE
    val state : Reference.t
  end

module type DICTIONARY_Instance =
  sig
    module Dictionary : DICTIONARY
    val state : Dictionary.t
  end

module type CHANNEL_Instance =
  sig
    module Channel : CHANNEL
    val state : Channel.t
  end
