(*
   Code simplifier:
   - statically evaluates expressions where possible
   - restructures code a bit
   Nik Sultana, Cambridge University Computer Lab, October 2015

   NOTE currently very naive

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open General
open Crisp_syntax
open Naasty
open State
open Naasty_aux


let rec simplify_expr (e : naasty_expression) : naasty_expression =
  match e with
  | And (Bool_Value b1, Bool_Value b2) -> Bool_Value (b1 && b2)
  | Or (Bool_Value b1, Bool_Value b2) -> Bool_Value (b1 || b2)
  | Not (Bool_Value b) -> Bool_Value (not b)
  | Plus (Int_Value i1, Int_Value i2) -> Int_Value (i1 + i2)
  | Minus (Int_Value i1, Int_Value i2) -> Int_Value (i1 - i2)
  | Times (Int_Value i1, Int_Value i2) -> Int_Value (i1 * i2)
  | Equals (Int_Value i1, Int_Value i2) -> Bool_Value (i1 = i2)
  | Equals (Bool_Value i1, Bool_Value i2) -> Bool_Value (i1 = i2)
  | Lt (Int_Value i1, Int_Value i2) -> Bool_Value (i1 < i2)
  | GEq (Int_Value i1, Int_Value i2) -> Bool_Value (i1 >= i2)
  | Gt (Int_Value i1, Int_Value i2) -> Bool_Value (i1 > i2)
  | LEq (Int_Value i1, Int_Value i2) -> Bool_Value (i1 <= i2)

  | Not e -> Not (simplify_expr e)
  | Abs e -> Abs (simplify_expr e)
  | Dereference e -> Dereference (simplify_expr e)
  | Address_of e -> Address_of (simplify_expr e)

  | Cast (ty, e) -> Cast (ty, simplify_expr e)

  | Field_In_Record (e1, e2) -> Field_In_Record (simplify_expr e1, simplify_expr e2)
  | And (e1, e2) -> And (simplify_expr e1, simplify_expr e2)
  | Or (e1, e2) -> Or (simplify_expr e1, simplify_expr e2)
  | Plus (e1, e2) -> Plus (simplify_expr e1, simplify_expr e2)
  | Equals (e1, e2) -> Equals (simplify_expr e1, simplify_expr e2)
  | Lt (e1, e2) -> Lt (simplify_expr e1, simplify_expr e2)
  | Minus (e1, e2) -> Minus (simplify_expr e1, simplify_expr e2)
  | Times (e1, e2) -> Times (simplify_expr e1, simplify_expr e2)
  | Mod (e1, e2) -> Mod (simplify_expr e1, simplify_expr e2)
  | Quotient (e1, e2) -> Quotient (simplify_expr e1, simplify_expr e2)
  | GEq (e1, e2) -> GEq (simplify_expr e1, simplify_expr e2)
  | Gt (e1, e2) -> Gt (simplify_expr e1, simplify_expr e2)
  | LEq (e1, e2) -> LEq (simplify_expr e1, simplify_expr e2)
  | ArrayElement (e1, e2) -> ArrayElement (simplify_expr e1, simplify_expr e2)
  | Left_shift (e1, e2) -> Left_shift (simplify_expr e1, simplify_expr e2)
  | Right_shift (e1, e2) -> Right_shift (simplify_expr e1, simplify_expr e2)

  | Call_Function (idx, template_parameters, es) ->
    Call_Function (idx, template_parameters,
                   List.map simplify_expr es)
  | _ -> e

let rec simplify_stmt (stmt : naasty_statement) : naasty_statement =
  match stmt with
  | Declaration (ty, e_opt, b) ->
    begin
      match e_opt with
      | None -> stmt
      | Some e' -> Declaration (ty, Some (simplify_expr e'), b)
    end
  | Seq (stmt1, stmt2) ->
    Seq (simplify_stmt stmt1, simplify_stmt stmt2)
  | Assign (Var x, Plus (Var y, Int_Value z)) ->
    if x = y then
      simplify_stmt (Increment (x, Int_Value z))
    else stmt
  | Assign (e1, e2) ->
    Assign (simplify_expr e1, simplify_expr e2)
  | Increment (idx, e) ->
    Increment  (idx, simplify_expr e)
  | For ((id, condition, increment), body) ->
    For ((id, simplify_expr condition, simplify_stmt increment),
         simplify_stmt body)

  | If (Bool_Value b, stmt1, stmt2) ->
    if b then stmt1 else stmt2
  | If1 (Bool_Value b, stmt) ->
    if b then stmt else Skip

  | If (e, stmt, Skip) ->
    simplify_stmt (If1 (simplify_expr e, simplify_stmt stmt))
  | If (e, Skip, stmt) ->
    simplify_stmt (If1 (simplify_expr (Not e), simplify_stmt stmt))
  | If (e, stmt1, stmt2) ->
    If (simplify_expr e, simplify_stmt stmt1, simplify_stmt stmt2)
  | If1 (e, stmt) ->
    If1 (simplify_expr e, simplify_stmt stmt)
  | St_of_E e ->
    St_of_E (simplify_expr e)
  | Switch (e, cases) ->
    Switch (simplify_expr e,
            List.map (fun (e, stmt) ->
              (e,(*We leave the guard alone*)
               simplify_stmt stmt)) cases)
  | Return e_opt ->
    begin
      match e_opt with
      | None -> stmt
      | Some e' -> Return (Some (simplify_expr e'))
    end

  | Label (s, stmt) ->
    Label (s, simplify_stmt stmt)
  | Commented (stmt, str) ->
    Commented (simplify_stmt stmt, str)

  | Skip
  | Break
  | Continue
  | GotoLabel _-> stmt
