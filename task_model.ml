(*
   Task and task graph metadata.
   Nik Sultana, Cambridge University Computer Lab, March 2015
*)

type task_type =
  | Input
  | Processing
  | Output

type chan_type =
  | Socket (*FIXME encode socket metadata -- address and port?*)
  | Type (*FIXME encode type data*)

type channel =
  {
    chan_id : int; (*FIXME needed?*)
    chan_type : chan_type;
  }

(*FIXME need inference to determine Task subclass? e.g., Many2One*)

type task =
  {
    task_id : int;
    task_type : task_type;
    (*NOTE channels in libNaaS are unidirectional*)
    input_channels : unit(*FIXME *);
    output_channels : unit(*FIXME as above*);
    process : Crisp_syntax.process;
  }

type task_graph = task list
