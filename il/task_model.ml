(*
   Task and task graph metadata.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

type task_type =
  | Input
  | Processing
  | Output

(*FIXME need inference to determine Task subclass? e.g., Many2One*)
(*FIXME since different subclasses have different constructors, and perhaps
        additional methods/members, each kind of class represented here must be
        associated with whatever additional methods/members/constructor
        parameters that class expects.*)
type task_class =
  | Many2One
  | Task
  (*FIXME incomplete*)

type chan_type =
  | Socket (*FIXME encode socket metadata -- address and port?*)
  | Type (*FIXME encode type data*)

(*NOTE a channel's id consists of its offset in input_chans or
       output_chans*)
type chan_offset = int
(*NOTE channels in libNaaS are unidirectional*)
type chan =
  {
    chan_type : chan_type;
    chan_id : int;
    (*FIXME incomplete?*)
  }

type task_id = int

type task =
  {
    task_id : task_id; (*NOTE must be unique*)
    task_type : task_type;
    task_class : task_class;
    input_chans : chan list;
    output_chans : chan list;
    process : Crisp_syntax.process;
  }

(*A connection describes which channel of which task (in a task graph) is
  connected to which channel of which (other) task.*)
type connection =
  { src : task_id * chan_offset;
    dst : task_id * chan_offset }

(*NOTE to be well-formed, all tasks ids mentioned in connections must be
       resolvable to tasks, and all chan offsets must be resolvable within their
       related tasks.*)
type task_graph =
  {
    tasks : task list;
    connections : connection list;
  }

(* Take a task and an integer representing a channel id and return the position of*)
(* the channel id in the array of input channels*)
let find_input_channel task channel =
  let rec find_it elt acc = function
    | hd :: tl when elt = hd.chan_id -> acc (* match *)
    | hd :: tl -> find_it elt (acc + 1) tl (* non-match *)
    | _ -> failwith ("Cannot find input_channel in channel list for task") (* end of list *)
  in find_it channel 0 task.input_chans 
    
  
let find_output_channel task channel =
  let rec find_it elt acc = function
    | hd :: tl when elt = hd.chan_id -> acc (* match *)
    | hd :: tl -> find_it elt (acc + 1) tl (* non-match *)
    | _ -> failwith ("Cannot find input_channel in channel list for task") (* end of list *)
  in find_it channel 0 task.output_chans  