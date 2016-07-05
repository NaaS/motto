(*
   Test of the scriptable runtime.
   Nik Sultana, Cambridge University Computer Lab, July 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open Runtime_inspect
open Runtime_data
open Crisp_syntax
open Resources

let c_fifo1 = Channel_resource (Resource_varieties.Channel.fifo "chan_fifo1")
let c_fifo2 = Channel_resource (Resource_varieties.Channel.fifo "chan_fifo2")
let _ = run [
  Acquire_Resource (c_fifo1, Some "my_fifo1");
  Acquire_Resource (c_fifo2, Some "my_fifo2");
  Declare_channel ("chan_fifo1", "integer/-");
  Declare_channel ("chan_fifo2", "-/integer");
(*  Eval "can chan_fifo";*)(*FIXME cannot "can" this sort of identifier!*)
  Assign_Resource ("chan_fifo1", c_fifo1);
  Assign_Resource ("chan_fifo2", c_fifo2);
  Eval "can chan_fifo1";
  Eval "can chan_fifo2";

  Load "tests/flick_code/process_forwarder.cp";
  Instantiate_Process ("forwarder", "Forwarder(chan_fifo1, chan_fifo2)");
  Run_Asynch;

  Dismiss_Resource c_fifo1;
  Dismiss_Resource c_fifo2;
  Eval "can chan_fifo1";
  Eval "can chan_fifo2";
  Unlink "chan_fifo1";
  Unlink "chan_fifo2";
]
