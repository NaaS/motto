(*
   Runtime state, specifically for a global worklist of concurrently-evaluated
     expressions, and process instances.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)


type chan_arg =
  Runtime_data.channel_direction *
  string (*channel name*)

type process_class_name = string
type process_instance_name = string

(*A process is presented for instantiation with unparsed actual parameters*)
type pre_process_instance =
  process_instance_name * process_class_name * chan_arg list * string list
(*When stored in the process_instance_list, the formal arguments are stored
  fully evaluated, since we don't want to re-evaluate them when the process is
  rescheduled*)
type process_instance =
  process_instance_name * (process_class_name * chan_arg list * Crisp_syntax.expression list)

type asynch_ctxt =
  {
    work_list : Eval_monad.work_item list;
    process_instance_list : process_instance list;
  }

let initial_asynch_ctxt =
  {
    work_list = [];
    process_instance_list = [];
  }
