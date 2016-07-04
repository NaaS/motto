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
module type CHANNEL_old =
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

(*
NOTE TRANSLATORs could be passed as parameters to other resources (that are made into
     functors) to map between the Flick level and the value stored by the resource.
FIXME not sure TRANSLATOR is needed any longer -- might have been subsumed by PARSER.
*)
module type TRANSLATOR =
  sig
    include RESOURCE

    (*FIXME where to put the argument/result of the translation?
            should reference a buffer?
            We could use an instance of REFERENCE for such.*)
    val to_ : t -> expression -> result
    val from_ : t -> expression -> result
  end

(*FIXME BUFFER originally didn't expose functions that read or write from "t".
        It was assumed that some other module will know enough about
        "t" to be able to encode/decode data appropriately.
        Maybe I could return to this model?
        The role of BUFFER is not to read/write into the buffer
        wrt the buffer's user; but rather it's intended to mediate
        between the buffer's user and the resource from which the
        buffered data is obtained.
        But perhaps having read/write functions in BUFFER is unavoidable?*)
module type BUFFER =
  sig
    type t
    (*Allocate a piece of memory of a certain size, with all values set to
      init_value if it's provided.*)
    (*FIXME can we source the buffer directly from a resource, and interact
            with the resource to make the buffer "zero-copy"?*)
    val create : ?init_value:int -> int -> t
    (*The size of the buffer. This is specified at creation time, and cannot
      be changed after that.*)
    val size : t -> int
    (*How much of the buffer is currently occupied.
      Naturally, occupied_size <= size.*)
    val occupied_size : t -> int
    (*Registers a function that will be called by "fill_until". This function
      can be updated by registering a different function later on.
      The function accepts two parameters (the offset that it will be instructed
      to start reading from, and the number of bytes it will
      attempt to read from a resource. It is expected that this resource will
      supply the filler function, which returns the number of bytes that it was able
      to read. The filler deposits the data it read (from the resource) into the
      buffer.
      Thus the filler function is the "bridge" between the resource we're
      reading from, and this buffer that we're using to store data that's
      read from a resource.*)
    val register_filler : t -> (bytes -> int(*start offset*) -> int(*bytes to read*) -> int(*bytes read*)) -> unit
    (*Attempts to fill the buffer until its occupied_size is as large as the
      given parameter. It can return immediately with "true" if this is already
      the case. Otherwise it will call the reader that's been registered with
      the buffer. If it can read sufficient bytes, then it returns "true",
      otherwise it returns "false" -- this is an indicator that "fill_until"
      should be called again (with the same parameters) until it returns "true"
      or until some timeout (handled separate, since it's unrelated to the
      buffer) expires.
      This function fails if the parameter is greater than size, or if a reader
      function has not yet been registered (using register_reader).*)
    val fill_until : t -> int -> bool
    (*FIXME need to add function for writing ("unfilling") into the resource,
            emptying the buffer.*)

    (*Read given number of bytes from the buffer.
      If the buffer contains fewer bytes than requested, then
      None is returned.*)
    val read : t -> int -> bytes option
    (*FIXME if i'm to keep "read", then perhaps should have
            a "write" and/or "append" function too.*)
  end

module type PARSER =
  sig
    type t
    (*We can be transparent about the buffer type since it's not abstract
      to the parser: it's overseen by a BUFFER module.*)
    type buffer
    (*Given a buffer, we create a parser that uses it for the rest of time.
      NOTE that the buffer would have had a reader registered with it
           by the resource, so the parser can just try to pull data from
           the buffer, which in turn try to pull data from the resource.*)
    val init : buffer -> t
    (*Return the buffer being used by a parser.
      FIXME not sure this is needed, since we probably don't want something
            else messing with our buffer*)
    val buffer : t -> buffer

(*NOTE i tried encapsulating a BUFFER inside a PARSER to redefine CHANNEL_BUILDER as
    module type CHANNEL_BUILDER = functor (Parser : PARSER) -> CHANNEL
instead of
    module type CHANNEL_BUILDER = functor (Parser_Fun : PARSER_BUILDER) (RX_Buffer : BUFFER) -> CHANNEL
I'd then have PARSER expose
    val rx_buffer_module : (module BUFFER)
which is implemented as
    let rx_buffer_module = (module Buffer : BUFFER)
and have
    module RX_Buffer = (val Parser.rx_buffer_module : BUFFER)
in instances of CHANNEL_BUILDER. But OCaml complained:
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
*)

    (*Parsing involves engaging with the buffer to ensure it contains
      the least amount of data we need in order to interpret (parse) it and
      decide whether it constitutes a result (in which case return it, introducing
      it into the Flick world), or continue parsing. In that case, we return
      Unavailable to signal that we had our go, and will continue when we're
      rescheduled later.*)
    val parse : t -> type_value -> result
    (*Unparsing involves serialising a Flick expression into bytes (in the
      parser's buffer) then writing out the buffer (using the BUFFER's API).
      If this suceeds then we return "true".
      If it fails because we cannot write (e.g., because the "write" to a socket
      couldn't write all the bytes out) then we return "false". This should show
      up at the Flick level as Unavailable, which means that the expression's
      evaluation in the Flick world blocked, and will be retried next time it's
      scheduled by the Evaluation Monad.
      FIXME currently we assume/require that compound expressions need to be written in a single go?
            and if that fails, then the whole expression will be retried later?
            does this make it more difficult to write compositional unparsers,
            because they'd need to undo any incomplete expressions that have
            started to be written out? can we wrap this in a transaction then
            to simplify? then the transaction will only succeed if all the writes
            it includes will succeed*)
    val unparse : t -> type_value -> expression -> bool
  end

(*FIXME how are the sizes of the TX and RX buffers specified? via "allocate"?*)
module type PARSER_BUILDER = functor (Buffer : BUFFER) -> PARSER with type buffer = Buffer.t

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

(*FIXME should the channel influence the sizes/dynamics of TX/RX buffers used by the parser?
        how are the sizes of the TX and RX buffers specified? via "allocate"?*)
module type CHANNEL_BUILDER = functor (Parser_Fun : PARSER_BUILDER) (RX_Buffer : BUFFER) -> CHANNEL

(*Bindings with functions executed externally.
  FIXME consider using this to abstract contents of front-end/functions.ml*)
(*FIXME encode the function's argument and return types?*)
module type FUNCTION =
  sig
    include RESOURCE

    val apply : t -> expression -> result
  end
module type FUNCTIONS =
  sig
    include RESOURCE

    val apply : t ->
     string (*Function name
              FIXME encode the function name as "expression" instead of "string"?*)
      -> expression -> result
  end
