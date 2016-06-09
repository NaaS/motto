(*
   Resources that can be used during runtime, to store data and interact with
   the environment.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


open Crisp_syntax
open Resource_types
open Resource_instances

type resource =
  (*FIXME there may be several kinds of channels, dictionaries, and references.
          should we use OCaml's class system for abstracting their interfaces?*)
  | Channel_resource (*FIXME how to deal with channel arrays?*)
  | Dictionary_resource of Dictionary.t
  | Reference_resource of Reference.t

let string_of_resource = function
  | Channel_resource -> "<channel resource>"
  | Dictionary_resource _ -> "<dictionary resource>"
  | Reference_resource _ -> "<reference resource>"

let acquire_resource (r : resource) (param : string option) : bool =
  match r with
  | Channel_resource -> failwith "TODO"
  | Dictionary_resource r -> Dictionary.initialise r param
  | Reference_resource r -> Reference.initialise r param

let dismiss_resource (r : resource) : bool =
  match r with
  | Channel_resource -> failwith "TODO"
  | Dictionary_resource r -> Dictionary.dismiss r
  | Reference_resource r -> Reference.dismiss r

let resource_is_available (r : resource) : bool =
  match r with
  | Channel_resource -> failwith "TODO"
  | Dictionary_resource r -> Dictionary.is_available r
  | Reference_resource r -> Reference.is_available r
