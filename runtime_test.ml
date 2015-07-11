(*
   Test of the scriptable runtime.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)

open Runtime_inspect
open State
open State_aux
open Runtime_data
open Crisp_syntax

let _ = run
  [Load "tests/simple_function.cp";
   Load "tests/maps_iterations2.cp";
   Declare_value ("zoo", "100");
   Set ("zoo", "200");
   Eval "zoo * 200";
(*   Eval "let zoo = 1300";*)
   Eval "f (zoo, 3 * zoo)";
   Eval "let l = [1,2,3,4]";
   Eval "let v = 1";
   Eval "let v' = 2";
   Eval "F (50)";
   Eval "let l' = F (f (v, v' + 1))";
   MI (Show_symbol_table None);
   MI (Show_runtime_ctxt None)]
