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

(*Resource conform to this basic interface, that provides the basic engagement
  of a resource. Note that the return type of the RESOURCE functions are not
  "result" (which would make those values directly visible at the Flick level).
  This is because most of the functionality provided by the RESOURCE interface
  is logistical. This is not entirely true for the "is_available" function,
  since this is often used to provide the value returned by "can" expressions.

  I thought to allow a callback function to be registered for when a resource is
  asynchronouslyadded/removed by the runtime system; the callback function would
  let the Flick level of the change in case a Flick-level name is mapped to a
  different dictionary or channel. But I decided that this didn't seem necessary.
*)
module type RESOURCE =
  sig
    (*Any identifiers and metadata that enable us to engage with a resource
      once it has been allocated and initialised*)
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

(*FIXME how are the sizes of the TX and RX buffers specified? via "allocate"?*)
module type CHANNEL =
  sig
    include RESOURCE
    (*What to attach the channel to.
      FIXME provide some facility to restrict the use of this function within
            Flick, to prevent the program logic from modifying the channels
            that have been assigned to it by the runtime system.
      FIXME currently we don't provide a way of calling this function from the
            Flick level. This could be provided via an expression such as
            "contact" for example.*)
    val attach_to : t -> expression -> result
    (*What are we connected to*)
    val attached_to : t -> result
    (*Register "callback functions". These consists of Flick expressions that
      are evaluated each time the corresponding event occurs.*)
    val on_attachment : t -> expression -> unit
    val on_detachment : t -> expression -> unit

    (*This next batch of functions provides semantics for the Flick-level "can"
      and "size" expressions.
      NOTE that for "peek" we use the "receive" equivalents -- "can_receive"
           or "size_receive"*)
    val can_receive : t -> result
    val size_receive : t -> result
    val can_send : t -> result
    val size_send : t -> result

    (*These functions provide semantics for the basic channel operators*)
    val peek : t -> result
    val receive : t -> result
    val send : t -> expression -> result
(*
NOTE for better performance we could batch "receive" and "send" requests and
     use functions such as those below, but this doesn't seem needed at the moment:
    val receive : t -> int -> result list
    val send : t -> expression list -> result
*)
  end
