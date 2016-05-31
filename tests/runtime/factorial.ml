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
   (*Evaluate this expression -- it simply binds "y" to a specific integer.*)
   Eval "let y = 13";

   (*Declare a channel in the runtime, and put some values into it.
     Note that since channels are bidirectional, we need to specify the
     direction -- that is "Incoming" in this case. That is, we are putting
     in values that can be read.*)
   Declare_channel ("somechan", "integer/-");
   Q_channel ("somechan", Incoming, None, "4");
   Q_channel ("somechan", Incoming, None, "2");

   (*Load and evaluate a Flick file -- this contains the factorial function.*)
   Load "tests/flick_code/factorial.cp";

   (*Bind z to an expression, that includes a read from a channel.*)
   Eval "let z = 5 + (y * ?somechan) + y";

   (*Apply the factorial function to an expression, that includes a read from a channel.*)
   (*NOTE that since this function grows quickly, and are carried by integers
          (which have a finite width) then giving large values to this function
          will cause overflow. This is unreported at the moment -- and just causes
          the integer to be set to 0. This will be fixed when exceptions are
          added to the evaluator.*)
   Eval "factorial (z - (60 + ?somechan))";

(* Enable this code to have the runtime system report its internal state at this point *)
(*
   MI (Show_symbol_table None);
   MI (Show_runtime_ctxt None)
*)
  ]

