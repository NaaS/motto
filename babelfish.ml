(*
   Translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open Crisp_syntax
open Naasty


type state =
  { pragmas : string list;
    type_declarations : naasty_type list;
    type_symbols : (string * integer) list
  }


(*FIXME i'm ignoring annotations for the time being*)
let rec naasty_of_flick_type (st : state) (declaration : bool)
  : (type_value -> naasty_type option * state) = function
  | Empty -> failwith "Cannot translate empty type"
  | UserDefinedType (label_opt, type_name) ->
    assert (not declaration);
    assert (List.mem_assoc type_name st.type_symbols);
    let type_name' = List.assoc type_name st.type_symbols in
    (*FIXME ignoring label*)
    let ty' =
      Some (UserDefined_Type (type_name', None(*FIXME*)))
    in (ty', st)
(*
  | String (label_opt, type_ann)
  | Integer (label_opt, type_ann)
  | Boolean (label_opt, type_ann)
  | RecordType (label_opt, tys, type_ann)
  | Disjoint_Union (label_opt, tys)
  | List (label_opt, ty, _, type_ann)
  | IPv4Address label_opt
  (*We send records, not tuples, over the wire, so tuples
    don't need type annotations.
    Also, tuples get translated into records.*)
  | Tuple (label_opt, tys)
  | Dictionary (label_opt, ty)
  | Reference (label_opt, ty) -> None, st
*)

