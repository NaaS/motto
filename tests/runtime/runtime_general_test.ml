(*
   Test of the scriptable runtime.
   Nik Sultana, Cambridge University Computer Lab, July 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
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
   Eval "let zoo_ = 1300";
   Eval "f (zoo, 3 * zoo)";
   Eval "let l = [1,2,3,4]";
   Eval "let v = 1";
   Eval "let v' = 2";
   Eval "F_map_iterations2 (50)";
   Eval "let l' = F_map_iterations2 (f (v, v' + 1))";

   Declare_channel ("somechan", "integer/boolean");
   Declare_channel ("somechan_array", "[integer/boolean]");
   Add_DI ("DI", "5");
   Declare_channel ("somechan_array_bounded", "[integer/boolean]{DI}");
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
   Eval "(if 3 < 4: boolchan ! True else: boolchan ! False) = (boolchan ! (3 < 4))";

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
   Eval "[? somechan, ? somechan, let x = ? somechan, ? somechan - zoo]";

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
   Eval "head (2 :: 3 .. 10)";
   (*List tail*)
   Eval "tail (2 :: 3 .. 10)";
   (*FIXME test "map"*)
   Eval "0 :: 1 .. 3";
   Eval "1 .. 3 @ 4 .. 10";
   Eval "[1 .. 3] @ [4 .. 10]";

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

   Eval ("hash(3)");
   Eval ("hash(3)");
   Eval ("hash(2)");

(* This would result in a type error, since we cannot assign to a non-reference
   Eval ("let blaX = 3");
   Eval ("blaX := 30");
*)

   Declare_reference ("blaX", "3");
   Eval ("blaX := 30");
   Eval ("blaX + 3");
   Eval ("blaX := (blaX + blaX) / 2");
(* This would result in an error since we'd be shadowing a definition
   Eval ("let blaX = 3");
*)

   Declare_dictionary ("dict", "integer", "integer");
   Eval ("dict[4] := 3");
   Eval ("dict[4]");
(*   Eval ("dict[3]"); This key has not yet been given a value in the
                       dictionary, so attempting to evaluate this expression
                       would break.*)
   Eval ("can dict[4]");
   Eval ("can dict[3]");

   Q_channel ("somechan", Incoming, None, "5");
   Eval "can ? somechan";
   Eval "can ? somechan_array_bounded[0]";
(*   Eval "can ? somechan_array_bounded"; FIXME currently this has no meaning*)
(*   Eval "can somechan_array_bounded"; FIXME currently this has no meaning*)

   Eval "can ? int_chan";
   Eval "can ? boolchan";
   Eval "can ? unitchan";
   Eval "can (unitchan ! <>)";

(*   Eval ("dict");  dictionaries cannot be evaluated*)
   Eval ("size dict");
   Eval ("size l");

   Eval "size ? somechan";
   Eval "size ? somechan_array_bounded[0]";
(*   Eval "size ? somechan_array_bounded"; FIXME currently this has no meaning*)
   Eval "size somechan_array_bounded";

   Eval "size ? int_chan";
   Eval "size ? boolchan";
   Eval "size ? unitchan";
   Eval "size (unitchan ! <>)";
   Eval "unitchan ! <>";
   Eval "size (unitchan ! <>)";

(*   Eval "3 / 0"; (*FIXME how to control this?*)*)

(*FIXME further test:
        exceptions, local and global state
        wiring up processes with channels
        channel arrays
  FIXME do we also need this asynch primitive: an expression is evaluated
        repeatedly, but if it blocks then it returns some other expression.
*)


   MI (Show_symbol_table None);
   MI (Show_runtime_ctxt None)]
;;

open Resources
let r = Reference_resource
  (Resource_varieties.Reference.local_reference 1)
let d = Dictionary_resource (Resource_instances.Dictionary.allocate (Some 10))
let _ = run [
  Acquire_Resource (r, Some "4");
  Declare_reference ("testref", "3");
  Eval "testref = 3";
  Assign_Resource ("testref", r);
  Eval "testref = 4";
  Eval "testref := 5";
  Eval "testref = 5";
  Eval "(can testref) = True"; (*This tells us that the resource is currently available*)
  Dismiss_Resource r;
(*  Eval "testref";*) (*We can no longer evaluate this since the resource has
                        been dismissed*)
  Eval "(can testref) = False"; (*This tells us that the resource is unavailable*)
  Unlink "testref";
(*  Eval "testref";*) (*We can no longer evaluate this since the resource has
                        is not even referenced in our runtime state.
  Eval "can testref"; -- Indeed we cannot even check to see if the resource is
                         available.*)

  Acquire_Resource (d, None);
  Declare_dictionary ("dict_res", "integer", "integer");
(*  Eval "can dict_res";*) (*FIXME currently doesn't work on 'local' dictionary. only works on 'external' dictionary.*)
  Eval "can dict_res[5]";
  Eval "dict_res[5] := 1";
  Eval "can dict_res[5]";
  Eval "dict_res[5]";
  Assign_Resource ("dict_res", d);
  Eval "can dict_res";
  Eval "can dict_res[5]";
  Eval "dict_res[5] := 2";
  Eval "dict_res[5]";
  Dismiss_Resource d;
  Eval "can dict_res[5]";
  Eval "can dict_res";
  Unlink "dict_res";
]

(*
(*FIXME is "capacity" actually used internally?*)
let capacity = 10
(*FIXME how to group channels together -- use OCaml classes?*)
let c_fifo = Channel_resource (Resource_instances.Channel_FIFO.allocate (Some capacity))
(*let c_timer = Channel_resource (Resource_instances.Channel_Timer.allocate (Some capacity))
let c_sock = Channel_resource (Resource_instances.Channel_Socket.allocate (Some capacity))
*)
let _ = run [
  Acquire_Resource (c_fifo, Some "path. or fd?"(*FIXME*));
(*  Acquire_Resource (c_timer, None);
  Acquire_Resource (c_sock, Some "fd?"(*FIXME*));
*)
  Declare_channel ("chan_fifo", "integer/integer");
(*  Declare_channel ("chan_timer", "integer/<>");
  Declare_channel ("chan_sock", "integer/integer");*)
  Assign_Resource ("chan_fifo", c_fifo);
(*  Assign_Resource ("chan_timer", c_timer);
  Assign_Resource ("chan_sock", c_sock);*)
  (*Check if channels are connected*)
  Eval "can chan_fifo";
(*  Eval "can chan_timer";
  Eval "can chan_sock";
  (*Set an alarm for 30 seconds from now*)
  Eval "chan_timer ! 30";
  (*FIXME Check if channels have space in rx/tx buffers*)
(*FIXME use Asynch_Eval for this block?*)
  Eval "chan_timer -> chan_timer ! 30";
  Eval "chan_timer <- chan_fifo ! True";

  (*Channel forwarding*)
  Eval "chan_fifo => chan_sock"
  Eval "chan_sock => chan_fifo"

  (*FIXME could write static analysis to detect overloaded forwarding (=>) or
          overloaded subscriptions (<- and ->) or overloaded listens (?) and
          sends (!) since these imply a race.*)
*)
  Dismiss_Resource c_fifo; (*
  Dismiss_Resource c_timer;
  Dismiss_Resource c_sock;*)
  Eval "can chan_fifo"; (*
  Eval "can chan_timer";
  Eval "can chan_sock";*)
  Unlink "chan_fifo";
  Unlink "chan_timer"; (*
  Unlink "chan_sock";*)
  (*NOTE might be interesting, but bordering on silly, to consider nested
         "can" expressions. For example, since we have unlinked chan_fifo,
         we now cannot eval "can chan_fifo" since there's no such thing as
         "chan_fifo" in our runtime state, so we don't know what "can" should
         mean in that context. But with nested "can" statements we would
         be able to eval "can (can chan_fifo)" and get "False".*)
]
*)
(*FIXME test channel arrays*)

(*FIXME can uncomment this -- but how to test more reliably?
let c_fifo = Channel_resource (Resource_instances.Channel_FIFO.allocate None)
let _ = run [
  Acquire_Resource (c_fifo, Some "my_fifo");
  Declare_channel ("chan_fifo", "integer/integer");
(*  Eval "can chan_fifo";*)(*FIXME cannot "can" this sort of identifier!*)
  Assign_Resource ("chan_fifo", c_fifo);
  Eval "can chan_fifo";
  Eval "? chan_fifo";

  Eval "can (? chan_fifo)";

  Dismiss_Resource c_fifo;
  Eval "can chan_fifo";
  Unlink "chan_fifo";
]
*)
