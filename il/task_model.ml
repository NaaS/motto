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

type graph_type =
  | ExplicitLinks of (task_id * task_id) list  (* Graph is an explicit list of all links 
                                                  task_id pairs represent links between tasks*)
  | FoldTree of (task_id * task_id * task_id * Crisp_syntax.dependency_index)  (*Graph is a merge from an input to an output using binary merge 
                                                  task_ids represent input task, processing task, output task
                                                    -- e.g. hadoop use case*)  
  | DeMux of (task_id * task_id * task_id * task_id * task_id * Crisp_syntax.dependency_index)  (* Graph takes input from a client and forwards
                                               it to one or more clients.  task_ids represent client input, client output,
                                              processing, backend input, backend output and number of backends e.g. mapreduce *)                                             

(*FIXME need inference to determine Task subclass? e.g., Many2One*)
(*FIXME since different subclasses have different constructors, and perhaps
        additional methods/members, each kind of class represented here must be
        associated with whatever additional methods/members/constructor
        parameters that class expects.*)
type task_class = int (*FIXME this is very information-poor. Wouldn't it be
                              helpful to have metadata, such as the class name?*)

type chan_id = int

type task =
  {
    task_id : task_id; (*NOTE must be unique*)
    task_type : task_type;
    task_class : task_class;
    input_chans : chan_id list;
    output_chans : chan_id list;
    process : Crisp_syntax.process;
  }

(*NOTE to be well-formed, all tasks ids mentioned in connections must be
       resolvable to tasks, and all chan offsets must be resolvable within their
       related tasks.*)
type task_graph =
  {
    tasks : task list;
    graph : graph_type;
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


(* Take a task and an integer representing a channel id and return the position of*)
(* the channel id in the array of input channels*)
let find_input_channel task channel =
  let rec find_it elt acc = function
    | hd :: tl when elt = hd -> acc (* match *)
    | hd :: tl -> find_it elt (acc + 1) tl (* non-match *)
    | _ -> failwith ("Cannot find input_channel " ^ string_of_int channel ^ " in channel list for task") (* end of list *)
  in find_it channel 0 task.input_chans 
    
  
let find_output_channel task channel =
  let rec find_it elt acc = function
    | hd :: tl when elt = hd -> acc (* match *)
    | hd :: tl -> find_it elt (acc + 1) tl (* non-match *)
    | _ -> failwith ("Cannot find output_channel " ^ string_of_int channel ^ " in channel list for task") (* end of list *)
  in find_it channel 0 task.output_chans
