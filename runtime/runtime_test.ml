(*
   Test of the scriptable runtime.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open Runtime_inspect
open Runtime_data
open Crisp_syntax

let _ = run [
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
   Eval "F_map_iterations2 (50)";
   Eval "let l' = F_map_iterations2 (f (v, v' + 1))";

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

(*FIXME test case-of, update, update-indexable, and indexable-projection
        wiring up processes with channels
        channel arrays
*)

   Load "tests/factorial.cp";
   Eval "factorial (5)";

   Asynch_Eval ("t1", "?somechan");
   Run_Asynch;

   Load "tests/print.cp";
   Asynch_Eval ("t2", "metaprint (<>)");
   Asynch_Eval ("t3", "somechan ! True");
   Run_Asynch;

   Declare_channel ("unitchan", "<>/<>");
   Load "tests/process_pair.cp";
(* FIXME this still doesn't work well
   Q_channel ("unitchan", Incoming, None, "<>");
   Asynch_Eval "unitchan ! <>";
   Instantiate_Process ("p1", "P1", [(Incoming, "unitchan")], []);
   Instantiate_Process ("p2", "P2", [(Outgoing, "unitchan")], []);
   Run_Asynch;
*)

   Declare_channel ("int_chan", "integer/integer");
   Q_channel ("int_chan", Incoming, None, "640");
   Load "tests/fun_chan.cp";
   (*NOTE that things block if we swap the next two lines.*)
   Eval "fun_chan(int_chan)";
   Eval "fun_chan(-int_chan)";
   (*NOTE the following shouldn't block if evaluated concurrently*)
   Asynch_Eval ("f1", "fun_chan(int_chan)");
   Asynch_Eval ("f2", "fun_chan(-int_chan)");
   Run_Asynch;

   (*Redeclaring a channel implicitly clears it*)
   Declare_channel ("int_chan", "integer/integer");
   Load "tests/fun_chan_simple.cp";
(*   Eval "fun_chan_simp2(-int_chan)";
   Eval "fun_chan_simp1(int_chan)";
*)
(*
   Asynch_Eval "fun_chan_simp1(int_chan)";
   Asynch_Eval "fun_chan_simp2(-int_chan)";
   Run_Asynch;
*)
(*   Eval "fun_chan_simp2(-int_chan)";*)
   Clear_Asynch_Eval;

   Asynch_Eval ("t4", "fun_chan_simp1(int_chan)");
   Asynch_Eval ("t5", "fun_chan_simp2(-int_chan)");
   Asynch_Eval ("t7", "fun_chan_simp1(int_chan)");

   Asynch_Eval ("t4.a", "fun_chan_simp1(int_chan)");
   Asynch_Eval ("t5.a", "fun_chan_simp2(-int_chan)");
   Asynch_Eval ("t7.a", "fun_chan_simp1(int_chan)");

   Asynch_Eval ("t6", "? int_chan");
   Asynch_Eval ("t6.b", "? int_chan");
(*   Asynch_Eval ("t6.c", "? int_chan");*)
   Asynch_Eval ("t8", "int_chan ! 1");
   Asynch_Eval ("t9", "int_chan ! 2");
   Asynch_Eval ("t10", "int_chan ! 3");
   Asynch_Eval ("t11", "int_chan ! 4");
   Asynch_Eval ("t12", "int_chan ! 5");
(*   Asynch_Eval "int_chan ! 6";*)
(*   Asynch_Eval "int_chan ! ? int_chan";*)
   Run_Asynch;

   Load "tests/fun_call.cp";
   Eval "fun_call_f2(6)";

   MI (Show_symbol_table None);
   MI (Show_runtime_ctxt None)]
