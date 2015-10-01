(*
   Test of the scriptable runtime.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open Runtime_inspect
open Runtime_data
open Crisp_syntax

let _ = run [
   Load "tests/flick_code/simple_function.cp";
   Load "tests/flick_code/maps_iterations2.cp";
   (*Load "tests/flick_code/simple_function.bad.cp";*)
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
   Eval "not (? boolchan)";
   Q_channel ("boolchan", Incoming, None, "False");
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "(not not ? boolchan) and False";
   Eval "not not ? boolchan or False";

(* NOTE will cause program to block, unless more input becomes available on that channel.
        Attempting to unblock this by sending it an ^C, interrupting it, will
        result in it evaluating to bottom (since no other value is logically
        possible). We cannot normalise bottom, and this results in the
        evaluation system faulting. This is by design; we could allow bottom to
        thread through the rest of the computation (e.g., if it's assigned to
        some variable) but then we'd face additional complications: we'd have to
        normalise, say, <5, _|_> to _|_, or tolerate expressions like <5, _|_>
        which would make our semantics non-strict.

   Eval "not (? boolchan)";
*)

   MI (Show_runtime_ctxt None);

   (*These next lines should evaluate to True, False, ... alternatingly*)
   Q_channel ("boolchan", Incoming, None, "False");
   Eval "((False or not ? boolchan) and True)";
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "((False or not ? boolchan) or False)";
   Q_channel ("boolchan", Incoming, None, "False");
   Eval "((not ? boolchan) or False)";
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "(False or (not ? boolchan))";
   Q_channel ("boolchan", Incoming, None, "False");
   Eval "(False or False or (not ? boolchan))";
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "(False or (False or (not ? boolchan)))";
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "False or (False or (? boolchan))";
   Q_channel ("boolchan", Incoming, None, "False");
   Eval "False or (False or (False or (? boolchan)))";
   Q_channel ("boolchan", Incoming, None, "False");
   Eval "not (False or (False or (? boolchan)))";

   Q_channel ("boolchan", Incoming, None, "True");
   Eval "((False or False) or (not ? boolchan))";
   Q_channel ("boolchan", Incoming, None, "False");
   Eval "not not not not not ? boolchan";
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "boolchan ! ((False or not ? boolchan) and True)";

   Q_channel ("boolchan", Incoming, None, "False");
   Eval "boolchan ! ((False or not ? boolchan) and True)";
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "boolchan ! (False or not ? boolchan) and True";

   (*"peek" on channel*)
   Q_channel ("boolchan", Incoming, None, "True");
   Eval "?? boolchan";
   Eval "?? boolchan";
   Eval "? boolchan";

   (*NOTE even if two expressions evaluate to the same value, their side-effects
          may be different. In this case, they're the same.*)
   Eval "(if 3 < 4: boolchan ! True else: boolchan ! False) = (boolchan ! 3 < 4)";

   (** Testing of some types other than Booleans **)

   Eval "<1, True, 1313>";

   Q_channel ("somechan", Incoming, None, "10");
   Q_channel ("somechan", Incoming, None, "100");
   Q_channel ("somechan", Incoming, None, "1000");
   (*Here we test tuple creation, then projection*)
   Eval "let test_tuple = <? somechan, ? somechan, ? somechan>";
   Eval "test_tuple.1";

   Load "tests/flick_code/records.cp";
   Eval "{ test = 3, a2 = True, b3 = <1, False, [3]>}";
   Q_channel ("somechan", Incoming, None, "5");
   Q_channel ("somechan", Incoming, None, "15");
   Q_channel ("somechan", Incoming, None, "115");
   Eval "{ test = ? somechan, a2 = True, b3 = <? somechan, False, [? somechan]>}";

   Q_channel ("somechan", Incoming, None, "5");
   Q_channel ("somechan", Incoming, None, "15");
   Q_channel ("somechan", Incoming, None, "115");
   Q_channel ("somechan", Incoming, None, "1115");
   (*Here we test record creation, then projection*)
   Eval "let testing = { test = ? somechan, a2 = True, b3 = <? somechan, False, [? somechan]>} with test = ? somechan";
   Eval "testing.test";

   Q_channel ("somechan", Incoming, None, "5");
   Q_channel ("somechan", Incoming, None, "15");
   Q_channel ("somechan", Incoming, None, "115");
   Q_channel ("somechan", Incoming, None, "1115");
   Eval "[? somechan, ? somechan, let x = ? somechan, ? somechan - x]";

   (*List comprehension*)
   Eval "1 .. 3";
   Eval "[1 .. 3]";
   (*List append*)
   Eval "[1, 2, 3] @ [4, 5, 6]";
   Eval "1 .. 3 @ 4 .. 10";
   Eval "[1 .. 3] @ [4 .. 10]";
   (*List cons*)
   Eval "2 :: 3 .. 10";
   Eval "1 :: 2 :: 3 .. 10";
   Eval "1 :: 2 :: 3 :: [4, 5, 6]";
   (*List head*)
   Eval "head (2 :: [3 .. 10])";
   (*List tail*)
   Eval "tail (2 :: [3 .. 10])";
   (*FIXME test "map"*)
   (*NOTE these are badly-typed; we don't use the type-checker in this interface yet*)
   Eval "0 :: [1 .. 3]";
   Eval "[1 .. 3] @ 4 .. 10";
   Eval "1 .. 3 @ [4 .. 10]";

   Load "tests/flick_code/variants.cp";
   Eval "F_variants(c(5))";
   Eval "F_variants(d(<>))";


  (** Testing of functions and asynch evaluation **)

   Load "tests/flick_code/factorial.cp";
   Eval "factorial (5)";

   Asynch_Eval ("t1", "?somechan");
   Run_Asynch;

   Load "tests/flick_code/print.cp";
   Asynch_Eval ("t2", "metaprint (<>)");
   Asynch_Eval ("t3", "somechan ! True");
   Run_Asynch;

   Declare_channel ("unitchan", "<>/<>");
   Load "tests/flick_code/process_pair.cp";

   Q_channel ("unitchan", Incoming, None, "<>");
   Asynch_Eval ("kick_off", "unitchan ! <>");
   Instantiate_Process ("p1", "P1(unitchan)");
   Instantiate_Process ("p2", "P2(-unitchan)");
   Run_Asynch;

   Declare_channel ("int_chan", "integer/integer");
   Q_channel ("int_chan", Incoming, None, "640");
   Load "tests/flick_code/fun_chan.cp";
   (*NOTE that things block if we swap the next two lines.*)
   Eval "fun_chan(int_chan)";
   Eval "fun_chan(-int_chan)";
   (*NOTE the following shouldn't block if evaluated concurrently*)
   Asynch_Eval ("f1", "fun_chan(int_chan)");
   Asynch_Eval ("f2", "fun_chan(-int_chan)");
   Run_Asynch;

   (*Redeclaring a channel implicitly clears it*)
   Declare_channel ("int_chan", "integer/integer");
   Load "tests/flick_code/fun_chan_simple.cp";
   Eval "fun_chan_simp2(-int_chan)";
   Eval "fun_chan_simp1(int_chan)";
   Eval "fun_chan_simp1(int_chan)";

   (*If we used Eval, evaluating the next two expressions in this order would
     block unless int_chan had incoming values.*)
   Asynch_Eval ("t-1", "fun_chan_simp1(int_chan)");
   Asynch_Eval ("t-2", "fun_chan_simp2(-int_chan)");
   Run_Asynch;

   (*At this point we should have a single incoming value left in int_chan.
     The next expression consumes it.*)
   Eval "fun_chan_simp1(int_chan)";

   (*This shouldn't be needed at this point.*)
   Clear_Asynch_Eval;

   Asynch_Eval ("t4", "fun_chan_simp1(int_chan)");
   Asynch_Eval ("t5", "fun_chan_simp2(-int_chan)");
   Asynch_Eval ("t7", "fun_chan_simp1(int_chan)");
   (*NOTE arbitrarily many expressions can share the same channel*)
   Asynch_Eval ("t4.a", "fun_chan_simp1(int_chan)");
   Asynch_Eval ("t5.a", "fun_chan_simp2(-int_chan)");
   Asynch_Eval ("t7.a", "fun_chan_simp1(int_chan)");
   Run_Asynch;

   Clear_Asynch_Eval;

   Asynch_Eval ("t8", "int_chan ! 1");
   Asynch_Eval ("t9", "int_chan ! 2");
   Asynch_Eval ("t10", "int_chan ! 3");
   Asynch_Eval ("t11", "int_chan ! 4");
   Asynch_Eval ("t12", "int_chan ! 5");
   Asynch_Eval ("t13", "int_chan ! 6");
   Q_channel ("int_chan", Incoming, None, "7");
   Asynch_Eval ("t14", "int_chan ! ? int_chan");
   Run_Asynch;

   Load "tests/flick_code/fun_call.cp";
   Eval "fun_call_f2(6)";

(*FIXME test:
        update, update-indexable, and indexable-projection*)
   Eval ("let blaX = 3");
   Eval ("blaX := 30"); (*FIXME we're updating an immutable*)
   Eval ("blaX[4]");
   Eval ("blaX[4] := 30");


(*FIXME further test:
        exceptions, local and global state
        wiring up processes with channels
        channel arrays
  FIXME do we also need this asynch primitive: an expression is evaluated
        repeatedly, but if it blocks then it returns some other expression.
*)


   MI (Show_symbol_table None);
   MI (Show_runtime_ctxt None)]
