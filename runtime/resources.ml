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
      of (module CHANNEL_Instance)
  | Dictionary_resource of (module DICTIONARY_Instance)
  | Reference_resource of (module REFERENCE_Instance)

(*FIXME could also print out the resource's initialisation string if it's been stored*)
let string_of_resource = function
  | Channel_resource (module R : CHANNEL_Instance) ->
      "<channel resource: " ^ R.name ^ ">"
  | Dictionary_resource (module R : DICTIONARY_Instance) ->
      "<dictionary resource: " ^ R.name ^ ">"
  | Reference_resource (module R : REFERENCE_Instance) ->
      "<reference resource: " ^ R.name ^ ">"

let acquire_resource (r : resource) (param : string option) : bool =
  match r with
  | Channel_resource (module R : CHANNEL_Instance) ->
      R.Channel.initialise R.state param
  | Dictionary_resource (module R : DICTIONARY_Instance) ->
      R.Dictionary.initialise R.state param
  | Reference_resource (module R : REFERENCE_Instance) ->
      R.Reference.initialise R.state param

let dismiss_resource (r : resource) : bool =
  match r with
  | Channel_resource (module R : CHANNEL_Instance) ->
      R.Channel.dismiss R.state
  | Dictionary_resource (module R : DICTIONARY_Instance) ->
      R.Dictionary.dismiss R.state
  | Reference_resource (module R : REFERENCE_Instance) ->
      R.Reference.dismiss R.state

let resource_is_available (r : resource) : bool =
  match r with
  | Channel_resource (module R : CHANNEL_Instance) ->
      R.Channel.is_available R.state
  | Dictionary_resource (module R : DICTIONARY_Instance) ->
      R.Dictionary.is_available R.state
  | Reference_resource (module R : REFERENCE_Instance) ->
      R.Reference.is_available R.state
