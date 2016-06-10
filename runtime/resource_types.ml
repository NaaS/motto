(*
   Resources that can be used during runtime, to store data and interact with
   the environment.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


open Crisp_syntax

(*There are two types of result we can get from a resource: a Flick expression
  (indicating that the resource was queried successfully) or an Error value.*)
type result =
    (*Normally the result consists of an expression*)
  | Expression of expression
    (*This is interpreted relative to the device or context.
      It might mean that there's nothing to be received on a
      channel. This value indicates transient and non-erroneous states.*)
    (*NOTE instead of polling for availability, could add a parameter indicating
           a minimum wait period.*)
    (*FIXME how to timeout, and how to handle this cleanly within the Eval module?*)
  | Unavailable
    (*This is interpreted relative to the device or context.
      It might mean that the channel's not open for sending, for
      example. This value indicates erroneous and
      non-transient circumstances.*)
  | Error of string

module type RESOURCE =
  sig
    type t

    (*Allocate a resource.
      Parameter: quantity of memory that the resource should be allocated,
      if this is a useful parameter to such a resource.*)
    val allocate : int option -> t

    (*Initialise the resource, thus making it available.*)
    val initialise : t -> string option -> bool
    val is_available : t -> bool

    (*Terminate the resource's use, mark it as unavailable to Flick, and allow
     the resource to be reclaimed or cleaned up.*)
    val dismiss : t -> bool
  end

module type REFERENCE =
  sig
    include RESOURCE

    val update : t -> expression -> result
    val retrieve : t -> result
  end

module type DICTIONARY =
  sig
    include RESOURCE

    val lookup : t -> expression -> result
    (*"update" is also used for "add"*)
    val update : t -> expression -> expression -> result
    val delete : t -> expression -> result
    val key_exists : t -> expression -> result
    val size : t -> result
    val capacity : t -> result
    val as_expression : t -> result
  end
