(*
   Defined functions in Flick.
   Nik Sultana, Cambridge University Computer Lab, September 2015
*)
(*FIXME ensure that interpreted fnctors never get declared by the user*)


open Crisp_syntax

exception Functions_Exc of string(*message*) * string(*function name*) * expression option

type function_entry =
  {
    (*Function name*)
    name : function_name;
    (*Function type*)
    ty : function_type;
    (*Function implementation (in OCaml)*)
    impl : expression list -> expression;
  }

type function_db = function_entry list

let function_db : function_db =
  [{name = "head";
    ty = FunType ([], FunDomType ([], [List (Some "l", def_undefined, None, [])]), FunRetType [def_undefined]);
    impl = (fun es ->
      match es with
      | [ConsList (h, _)] -> h
      | [e] -> failwith "Function not defined for given input"(*FIXME give more details*)
      | _ -> failwith "Wrong number of arguments"(*FIXME use Functions_Exc*)
      )
   };
   {name = "tail";
    ty = FunType ([], FunDomType ([], [List (Some "l", def_undefined, None, [])]), FunRetType [List (None, def_undefined, None, [])]);
    impl = (fun es ->
      match es with
      | [ConsList (_, t)] -> t
      | [e] -> failwith "Function not defined for given input"(*FIXME give more details*)
      | _ -> failwith "Wrong number of arguments"(*FIXME use Functions_Exc*)
      )
   };
   {name = "hash";
    ty = FunType ([], FunDomType ([], [def_undefined]), FunRetType [Integer (None, [])]);
    impl = (fun x -> Int (Hashtbl.hash x))
   };
  ]

let export_fun (e : function_entry) : (function_name * (bool * function_type)) =
  (e.name, (true, e.ty))

let export_funs : (function_name * (bool * function_type)) list =
  List.map export_fun function_db

(*
let lookup_function (fn : function_name) : function_entry option =
  let result =
    List.filter (fun e -> e.name = fn) function_db in
  match result with
  | [] -> None
  | [e] -> Some e
  | _ -> raise (Functions_Exc ("Found multiple definitions", fn, None))
*)
let apply_function (fn : function_name) (args : expression list) : expression =
  let result =
    List.filter (fun e -> e.name = fn) function_db in
  match result with
  | [] -> raise (Functions_Exc ("Could not find definitions", fn, None))
  | [e] -> e.impl args
  | _ -> raise (Functions_Exc ("Found multiple definitions", fn, None))
