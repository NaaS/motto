(*
   Translation from Flick to the NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open Crisp_syntax
open Naasty


type state =
  { pragmas : string list;
    type_declarations : naasty_type list;
    next_typesymbol : integer;
    type_symbols : (string * integer) list;
    next_symbol : integer;
    symbols : (string * integer) list;
  }

(*FIXME i'm ignoring annotations for the time being*)
let rec naasty_of_flick_type (st : state)
  : (type_value -> naasty_type * state) = function
  | Disjoint_Union (_, _) -> failwith "Unsupported"
  | Empty -> failwith "Cannot translate empty type"
  | Tuple (label_opt, []) ->
    assert (label_opt = None);
    (*We cannot have values of type "void" in the target, we can only type
      functions with such a type.*)
    (Unit_Type, st)
  | UserDefinedType (label_opt, type_name) ->
    if not (List.mem_assoc type_name st.type_symbols) then
      failwith ("Undeclared type: " ^ type_name);
    let type_name' = List.assoc type_name st.type_symbols in
    let (label_opt', st') =
      begin
        match label_opt with
        | None -> (None, st)
        | Some s ->
          if (List.mem_assoc s st.symbols) then
            (*shadowing is forbidden*)
            failwith ("Already declared: " ^ s);
          (Some st.next_symbol,
           { st with
             symbols = (s, st.next_symbol) :: st.symbols;
             next_symbol = 1 + st.next_symbol;
           })
      end in
    let ty' =
      UserDefined_Type (label_opt', type_name')
    in (ty', st')
(*
  | String (label_opt, type_ann)
  | Integer (label_opt, type_ann)
  | Boolean (label_opt, type_ann)
  | RecordType (label_opt, tys, type_ann)
  | List (label_opt, ty, _, type_ann)
  | IPv4Address label_opt
  (*We send records, not tuples, over the wire, so tuples
    don't need type annotations.
    Also, tuples get translated into records.*)
  | Tuple (label_opt, tys)
  | Dictionary (label_opt, ty)
  | Reference (label_opt, ty) -> None, st
*)


(*FIXME crude test
Record_Type (0, [(1, Int_Type {signed = true; precision = 32});
                 (2, Bool_Type);
                 (3, String_Type);
                 (4, Array_Type (Int_Type {signed = false; precision = 64},
                                 Some 4))])
|> string_of_naasty_type prog_indentation true
  |> print_endline
*)
