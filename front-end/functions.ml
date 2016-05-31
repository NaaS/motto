(*
   Defined functions in Flick.
   Nik Sultana, Cambridge University Computer Lab, September 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
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

let icl_backend_string_comparison =
  (*FIXME put this in an ICL-specific module*)
  "StringUtility::DiffStrLessThan"

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
   {name = "bind";
    ty = FunType ([], FunDomType ([], [def_undefined; Undefined "Y"]), FunRetType [flick_unit_type]);
    impl = (fun x -> failwith "TODO")
   };
   {name = icl_backend_string_comparison;
    ty = FunType ([], FunDomType ([], [String (Some "s1", []); String (Some "s2", [])]),
                  FunRetType [Boolean (None, [])]);
    impl = (fun x -> failwith "This function is only implemented in the ICL backend")
   };
   {name = "src_address";
    ty = FunType ([], FunDomType ([], [def_undefined]), FunRetType [Integer (None, [])]);
    impl = (fun x -> failwith "TODO")
   };
   {name = "reinterpret_cast";(*FIXME unused*)
    ty = FunType
           ([], FunDomType
                  ([], [Undefined "diffingo_type";(*FIXME this should be a
                                                    template parameter, but the
                                                    AST does not provide a way of
                                                    encoding such info*)
                        Undefined "chan_contents"]),
                  FunRetType [flick_unit_type]);
    impl = (fun x -> failwith "TODO")
   };
  ]

let export_fun (e : function_entry) : (function_name * (bool * function_type)) =
  (e.name, (true, e.ty))

let export_funs : (function_name * (bool * function_type)) list =
  List.map export_fun function_db

let apply_function (fn : function_name) (args : expression list) : expression =
  let result =
    List.filter (fun e -> e.name = fn) function_db in
  match result with
  | [] -> raise (Functions_Exc ("Could not find definitions", fn, None))
  | [e] -> e.impl args
  | _ -> raise (Functions_Exc ("Found multiple definitions", fn, None))
