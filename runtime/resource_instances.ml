(*
   Resources that can be used during runtime, to store data and interact with
   the environment.
   Nik Sultana, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


open Crisp_syntax
open Resource_types

(*Instead of un/parsing expressions, we store them directly in this example.*)
(*FIXME implement example involving remote reference?*)
module Reference : REFERENCE =
struct
  type t = bool ref * expression ref

  let allocate (Some n) =
    (*FIXME we currently allow storing expressions of any size in the reference!*)
    assert (n = 1);
    (ref false, ref Bottom)

  let initialise (_r_assigned, _r) s_opt =
    assert (not !_r_assigned);
    match s_opt with
    | None -> failwith "Initial value required"
    | Some e_s ->
      begin
        let e =
          match Crisp_parse.parse_string ("(| " ^ e_s ^ "|)") with
          | Expression e -> e
          | _ ->
            failwith ("Could not parse into an expression: " ^ e_s) in
        _r := e;
      end;
    _r_assigned := true;
    !_r_assigned

  let is_available (_r_assigned, _) =
    !_r_assigned

  let update (_r_assigned, _r) e =
    assert !_r_assigned;
    _r := e;
    Expression e

  let retrieve (_r_assigned, _r) =
    assert !_r_assigned;
    Expression !_r

  let dismiss (_r_assigned, _) =
    assert !_r_assigned;
    _r_assigned := false;
    not (!_r_assigned)
end

module Dictionary : DICTIONARY =
struct
  type t = {
    availability : bool ref;
    capacity : int;
    (*NOTE we'd prefer to use 'typed_value' not 'expression' here, but with the
           current code it creates a circular dependency among modules. In the
           reference above we stored 'expression'. The choice is a bit
           arbitrary, but using typed_value adds extra protection to ensure that
           we only store stuff that's "storable" in Flick.*)
    content : (expression, expression) Hashtbl.t;
  }

  let allocate (Some n) = {
    availability = ref true;
    capacity = n;
    content = Hashtbl.create ~random:false n;
  }

  let initialise t s_opt =
    (*We initialised the dictionary durine 'allocate' above*)
    assert (s_opt = None);
    !(t.availability)

  let is_available t = !(t.availability)

  let dismiss t =
    t.availability := false;
    not (!(t.availability))

  let lookup t k_e =
    assert !(t.availability);
    Expression (Hashtbl.find t.content k_e)

  let update t k_e v_e =
    assert !(t.availability);
    assert (Hashtbl.length t.content < t.capacity);
    Hashtbl.replace t.content k_e v_e;
    Expression v_e

  let delete t k_e =
    assert !(t.availability);
    Hashtbl.remove t.content k_e;
    Expression Crisp_syntax.flick_unit_value

  let key_exists t k_e =
    assert !(t.availability);
    let result =
      Hashtbl.mem t.content k_e
      |> Crisp_syntax_aux.lift_bool in
    Expression result

  let size t =
    assert !(t.availability);
    Expression (Int (Hashtbl.length t.content))

  let capacity t =
    assert !(t.availability);
    Expression (Int (t.capacity))

  let as_expression t =
    failwith "TODO"
end
