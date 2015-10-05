(*
   Task and task graph metadata.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

open Crisp_syntax
open General

type task_type =
  | Input
  | ManyToOne
  | AnyToOne
  | Output

type task_id = int

type graph_hint =   (* Type of task graph we are constructing *)
  | PassThroughType
  | FoldTreeType
  | DeMuxType                                             


type chan_id = int

type task =
  {
    task_id : task_id; (*NOTE must be unique*)
    task_type : task_type;
    process : Crisp_syntax.process;
  }

(*NOTE to be well-formed, all tasks ids mentioned in connections must be
       resolvable to tasks, and all chan offsets must be resolvable within their
       related tasks.*)
type task_graph =
  {
    tasks : task list;
  }

(* input_map takes a string that represents the channel and optionally an*)
let input_map (chan_name : string) (index : expression option) (hint : graph_hint) : expression =
  match hint with 
    | PassThroughType -> Int 0
    | FoldTreeType ->  the index 
    | DeMuxType -> begin match chan_name with 
      | "client_in" -> Int 0
      | "backend_in" ->  Plus ((Int 1),(the index))
      | _ -> failwith ("Expected channel name to be one from client or backend")
      end
  
(* output_map takes a string that represents the channel and optionally an*)
let output_map (chan_name : string) (index : expression option) (hint : graph_hint) : expression =
  match hint with 
    | PassThroughType -> Int 0
    | FoldTreeType -> Int 0
    | DeMuxType -> begin match chan_name with 
      | "client_out" -> Int 0
      | "backend_out" -> Plus ((Int 1),(the index))
      | _ -> failwith ("Expected channel name to be one from client or backend")
      end  
