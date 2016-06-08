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
  | Unavailable
    (*This is interpreted relative to the device or context.
      It might mean that the channel's not open for sending, for
      example. This value indicates erroneous and
      non-transient circumstances.*)
  | Error of string

type reference =
  {
    initialise : string option -> bool;
    is_available : unit -> bool;

    update : expression -> result;
    retrieve : unit -> result;

    dismiss : unit -> bool;
  }

type resource =
  | Channel_resource (*FIXME how to deal with channel arrays?*)
  | Dictionary_resource
  | Reference_resource of reference

let string_of_resource = function
  | Channel_resource -> "<channel resource>"
  | Dictionary_resource -> "<dictionary resource>"
  | Reference_resource _ -> "<reference resource>"

let acquire_resource (r : resource) (param : string option) : bool =
  match r with
  | Channel_resource
  | Dictionary_resource -> failwith "TODO"
  | Reference_resource r ->
    r.initialise param

let dismiss_resource (r : resource) : bool =
  match r with
  | Channel_resource
  | Dictionary_resource -> failwith "TODO"
  | Reference_resource r -> r.dismiss ()

let resource_is_available (r : resource) : bool =
  match r with
  | Channel_resource
  | Dictionary_resource -> failwith "TODO"
  | Reference_resource r -> r.is_available ()
