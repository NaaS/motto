(*
   Resources that can be used during runtime, to store data and interact with
   the environment.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


open Crisp_syntax
open Resource_types
open Resource_instances
open Resource_instance_wrappers

type resource =
  | Channel_resource (*FIXME how to deal with channel arrays?*)
      of Channel_FIFO.t
  | Dictionary_resource of Dictionary.t
  | Reference_resource of (module REFERENCE_Instance)

let string_of_resource = function
  | Channel_resource _ -> "<channel resource>"
  | Dictionary_resource _ -> "<dictionary resource>"
  | Reference_resource _ -> "<reference resource>"

let acquire_resource (r : resource) (param : string option) : bool =
  match r with
  | Channel_resource r -> Channel_FIFO.initialise r param
  | Dictionary_resource r -> Dictionary.initialise r param
  | Reference_resource (module R : REFERENCE_Instance) ->
      R.Reference.initialise (General.the R.state) param

let dismiss_resource (r : resource) : bool =
  match r with
  | Channel_resource r -> Channel_FIFO.dismiss r
  | Dictionary_resource r -> Dictionary.dismiss r
  | Reference_resource (module R : REFERENCE_Instance) ->
      R.Reference.dismiss (General.the R.state)

let resource_is_available (r : resource) : bool =
  match r with
  | Channel_resource r -> Channel_FIFO.is_available r
  | Dictionary_resource r -> Dictionary.is_available r
  | Reference_resource (module R : REFERENCE_Instance) ->
      R.Reference.is_available (General.the R.state)
