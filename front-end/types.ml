(*
   Defined types.
   Nik Sultana, Cambridge University Computer Lab, October 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)
(*FIXME ensure that interpreted symbols never get declared by the user*)
(*FIXME might want to include backend-specific meaning for these in relevant
        backend modules only.*)


open Crisp_syntax
open Naasty

exception Types_Exc of string(*message*) * string(*function name*) * expression option

type type_entry =
  {
    (*Type name*)
    name : label;
    (*Flick type*)
    ty_opt : type_value option;
    (*IL type*)
    naasty_ty_opt : naasty_type option;
  }

let task_event_ty = Literal_Type (None, "TaskEvent")

let types =
  [
    let naasty_ty = Literal_Type (None, "TaskEvent") in
    { name = "TaskEvent";
      ty_opt = Some (IL_Type naasty_ty);
      naasty_ty_opt = Some naasty_ty;
    };
  ]

let export_types =
  List.map (fun {name; (*ty_opt;*) naasty_ty_opt} -> (name, naasty_ty_opt)) types
