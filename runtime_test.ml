(*
   Test of the scriptable runtime.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open Runtime_inspect
open Runtime_data
open Crisp_syntax

let _ = run [
(*
   Load "tests/simple_function.cp";
   Load "tests/maps_iterations2.cp";
   (*Load "tests/simple_function.bad.cp";*)
   Declare_value ("zoo", "100");
   Set ("zoo", "200");
   Eval "zoo * 200";
   Eval "let zoo = 1300";
   Eval "f (zoo, 3 * zoo)";
   Eval "let l = [1,2,3,4]";
   Eval "let v = 1";
   Eval "let v' = 2";
   Eval "F (50)";
   Eval "let l' = F (f (v, v' + 1))";
*)
   Declare_channel ("somechan", "integer/boolean");
   Declare_channel ("somechan_array", "[integer/boolean]");
   Q_channel ("somechan", Incoming, None, "40");
   Q_channel ("somechan", Incoming, None, "400");
   Q_channel ("somechan", Outgoing, None, "False and True");
   (*Eval "true => somechan";*)
   Eval "somechan ! True";
   MI (Show_runtime_ctxt None);
   Eval "somechan ! (if ? somechan = 40: False else: True) and True";
   Deq_channel ("somechan", Incoming, None);

   Declare_channel ("boolchan", "boolean/boolean");
   Q_channel ("boolchan", Incoming, None, "True");
   Q_channel ("boolchan", Incoming, None, "False");
(*   Q_channel ("boolchan", Incoming, None, "True");*)
   Eval "True";
   Eval "False";
   Eval "not True";
   Eval "not False";
   Eval "? boolchan";
(*   Eval "not (? boolchan)";*)
(*   Eval "(not not ? boolchan) and False";*)
(*   Eval "not not ? boolchan and False";*)

(* NOTE will cause program to block, unless more input becomes available on that channel.
   Eval "not (? boolchan)";
   Eval "not (? boolchan)";
   Eval "not (? boolchan)";
   Eval "not (? boolchan)";
*)

   MI (Show_runtime_ctxt None);
(*   Eval "((False or not ? boolchan) and True)";*)
(*   Eval "((False or not ? boolchan) or False)";*)
(*   Eval "((not ? boolchan) or False)";*)
(*   Eval "(False or (not ? boolchan))";*)
(*   Eval "(False or False or (not ? boolchan))";*)
(*   Eval "(False or (False or (not ? boolchan)))";*)
(*   Eval "False or (False or (? boolchan))";*)
(*   Eval "False or (False or (False or (? boolchan)))";*)
(*   Eval "not (False or (False or (? boolchan)))";*)

(*   Eval "((False or False) or (not ? boolchan))";*)
(*   Eval "not not not not not ? boolchan";*)

   Eval "boolchan ! ((False or not ? boolchan) and True)";
(*
   Eval "boolchan ! ((False or not ? boolchan) and True)";
   Eval "boolchan ! (False or not ? boolchan) and True";
*)

   Eval "<1, True, 1313>";

   Q_channel ("somechan", Incoming, None, "10");
   Q_channel ("somechan", Incoming, None, "100");
   Q_channel ("somechan", Incoming, None, "1000");
   Eval "<? somechan, ? somechan, ? somechan>";

   Load "tests/records.cp";
   Eval "{ test = 3, a2 = True, b3 = <1, False, [3]>}";
   Q_channel ("somechan", Incoming, None, "5");
   Q_channel ("somechan", Incoming, None, "15");
   Q_channel ("somechan", Incoming, None, "115");
   Eval "{ test = ? somechan, a2 = True, b3 = <? somechan, False, [? somechan]>}";

   Q_channel ("somechan", Incoming, None, "5");
   Q_channel ("somechan", Incoming, None, "15");
   Q_channel ("somechan", Incoming, None, "115");
   Q_channel ("somechan", Incoming, None, "1115");
   Eval "let testing = { test = ? somechan, a2 = True, b3 = <? somechan, False, [? somechan]>} with test = ? somechan";
   Eval "testing.test";

   Q_channel ("somechan", Incoming, None, "5");
   Q_channel ("somechan", Incoming, None, "15");
   Q_channel ("somechan", Incoming, None, "115");
   Q_channel ("somechan", Incoming, None, "1115");
   Eval "[? somechan, ? somechan, let x = ? somechan, ? somechan - x]";

   Load "tests/variants.cp";

(*FIXME test case-of, map, iteration, function call,
        update, update-indexable, and indexable-projection*)

   MI (Show_symbol_table None);
   MI (Show_runtime_ctxt None)]
