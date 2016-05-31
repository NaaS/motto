(*
   Supporting functions for definiting guarded transformations on the
   NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, October 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

open General
open Naasty
open State

type stmt_transformation_guard = state -> naasty_statement -> bool

type stmt_transformation = state -> naasty_statement -> naasty_statement * state

let rec guard_stmt_transformation (g : stmt_transformation_guard)
      (tr : stmt_transformation) (st : state) (stmt : naasty_statement) : naasty_statement * state =
  if g st stmt then tr st stmt
  else
    (*Traverse the statement, and try applying the transformation to its
     sub-statements*)
    match stmt with
    | Seq (stmt1, stmt2) ->
      let stmt1', st' = guard_stmt_transformation g tr st stmt1 in
      let stmt2', st'' = guard_stmt_transformation g tr st' stmt2 in
      Seq (stmt1', stmt2'), st''
    | If (e, stmt1, stmt2) ->
      let stmt1', st' = guard_stmt_transformation g tr st stmt1 in
      let stmt2', st'' = guard_stmt_transformation g tr st' stmt2 in
      If (e, stmt1', stmt2'), st''
    | If1 (e, stmt1) ->
      let stmt1', st' = guard_stmt_transformation g tr st stmt1 in
      If1 (e, stmt1'), st'
    | Commented (stmt1, s) ->
      let stmt1', st' = guard_stmt_transformation g tr st stmt1 in
      Commented (stmt1', s), st'
    | Label (l, stmt1) ->
      let stmt1', st' = guard_stmt_transformation g tr st stmt1 in
      Label (l, stmt1'), st'
    | For (((id, id_init), condition, increment_stmt), body_stmt) ->
      let increment_stmt', st' = guard_stmt_transformation g tr st increment_stmt in
      let body_stmt', st'' = guard_stmt_transformation g tr st' body_stmt in
      For (((id, id_init), condition, increment_stmt'), body_stmt'), st''
    | Switch (e, cases) ->
      let cases', st' =
        fold_map ([], st) (fun st (e, stmt) ->
          let stmt', st' = guard_stmt_transformation g tr st stmt in
          (e, stmt'), st') cases in
      Switch (e, cases'), st'

    (*The remaining forms of statements do not have sub-statements*)
    | Declaration _
    | Assign _
    | Increment _
    | Break
    | Continue
    | Return _
    | Skip
    | St_of_E _
    | GotoLabel _ -> stmt, st
