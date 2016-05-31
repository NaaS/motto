(*
   Task and task graph metadata.
   Nik Sultana, Cambridge University Computer Lab, March 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
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
