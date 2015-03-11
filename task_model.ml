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

(*NOTE channels in libNaaS are unidirectional*)
type chan =
  {
    (*NOTE a channel's id consists of its offset in input_chans or
           output_chans*)
    chan_type : chan_type;
  }

type task =
  {
    task_id : int; (*NOTE must be unique*)
    task_type : task_type;
    task_class : task_class;
    input_chans : chan list;
    output_chans : chan list;
    process : Crisp_syntax.process;
  }

type task_graph = task list(*FIXME incomplete*)
