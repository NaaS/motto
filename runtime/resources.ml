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
    val allocate : int -> t

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

(*Instead of un/parsing expressions, we store them directly in this example.*)
(*FIXME implement example involving remote reference?*)
module Reference : REFERENCE =
struct
  type t = bool ref * expression ref

  let allocate n =
    (*FIXME we currently allow storing expressions of any size in the reference!*)
    assert (n = 1);
    (ref false, ref Bottom)

  let initialise (_r_assigned, _r) s_opt =
    assert (not !_r_assigned);
    match s_opt with
    | None -> failwith "Initial value required"
    | Some e_s ->
      begin
        let e =
          match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
          | Expression e -> e
          | _ ->
            failwith ("Could not parse into an expression: " ^ e_s) in
        _r := e;
      end;
    _r_assigned := true;
    !_r_assigned

  let is_available (_r_assigned, _) =
    !_r_assigned

  let update (_r_assigned, _r) e =
    assert !_r_assigned;
    _r := e;
    Expression e

  let retrieve (_r_assigned, _r) =
    assert !_r_assigned;
    Expression !_r

  let dismiss (_r_assigned, _) =
    assert !_r_assigned;
    _r_assigned := false;
    not (!_r_assigned)
end

type resource =
  | Channel_resource (*FIXME how to deal with channel arrays?*)
  | Dictionary_resource
  | Reference_resource of Reference.t

let string_of_resource = function
  | Channel_resource -> "<channel resource>"
  | Dictionary_resource -> "<dictionary resource>"
  | Reference_resource _ -> "<reference resource>"

let acquire_resource (r : resource) (param : string option) : bool =
  match r with
  | Channel_resource
  | Dictionary_resource -> failwith "TODO"
  | Reference_resource r ->
    Reference.initialise r param

let dismiss_resource (r : resource) : bool =
  match r with
  | Channel_resource
  | Dictionary_resource -> failwith "TODO"
  | Reference_resource r -> Reference.dismiss r

let resource_is_available (r : resource) : bool =
  match r with
  | Channel_resource
  | Dictionary_resource -> failwith "TODO"
  | Reference_resource r -> Reference.is_available r
