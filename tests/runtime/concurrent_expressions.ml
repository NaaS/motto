(*
   Demo of the scriptable runtime.
   Nik Sultana, Cambridge University Computer Lab, July 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open Runtime_inspect
open Runtime_data
open Crisp_syntax

let _ =
 run [
   (*Declare a channel in the runtime, and put a value into it.
     Note that since channels are bidirectional, we need to specify the
     direction -- that is "Incoming" in this case. That is, we are putting
     in a value that can be received.*)
   Declare_channel ("int_chan", "integer/integer");
   Q_channel ("int_chan", Incoming, None, "6");

   (*Load the file containing the "fun_chan" function. We will apply this
     function in different ways. The function takes a channel parameter. This
     channel must be able to carry integers in both directions -- we will use
     the "int_chan" channel declared earlier.*)
   Load "tests/flick_code/fun_chan.cp";

   (*First, we will evaluate two applications of this function in sequence.
     Evaluation is blocking.*)
   (*NOTE that things block if we swap the next two lines.*)
   Eval "fun_chan(int_chan)";
   (*Note that in the next application of the function, the (channel) parameter
     is preceded by "-". This indicates that we form the conjugate of the
     channel -- the channel with inverted directions.*)
   Eval "fun_chan(-int_chan)";

   (*Next, we will evaluate the two applications of this function concurrently.
     We do this by specifying them one by one, giving them names.
     "f2" is a straightforward application of this function.
     "f1" is a nested application of this function. "f1" depends on "f2".
     The evaluation doesn't happen immediately; evaluation takes place when
     we encounter Run_Asynch.*)
   (*NOTE the following shouldn't block if evaluated concurrently*)
   Asynch_Eval ("f2", "fun_chan(-int_chan)");
   Load "tests/flick_code/factorial.cp";
   Asynch_Eval ("f1", "factorial (4 + fun_chan(int_chan))");
   Run_Asynch;

(* Enable this code to have the runtime system report its internal state at this point *)
(*
   MI (Show_symbol_table None);
   MI (Show_runtime_ctxt None)
*)
  ]

