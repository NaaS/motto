(*
   Runtime state, specifically for a global worklist of concurrently-evaluated
     expressions, and process instances.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)


type chan_arg =
  Runtime_data.channel_direction *
  string (*channel name*)
type arg = string (*expression, unparsed*)

type process_class_name = string
type process_instance_name = string

type process_instance =
  process_instance_name * process_class_name * chan_arg list * arg list

type asynch_ctxt =
  {
    work_list : Eval_monad.eval_monad list;
    process_instance_list : process_instance list;
  }

let initial_asynch_ctxt =
  {
    work_list = [];
    process_instance_list = [];
  }
