(*
   Collection of functions used across the code base for debugging-related activities.
   Nik Sultana, Cambridge University Computer Lab, July 2015
*)


(*More of e debugging-related function, than a general one*)
let print_list indentation l =
  let sep = "\n" ^ indentation ^ "\t" in
  if l = [] then "(empty)"
  else sep ^ String.concat sep l

(*This is used to avoid committing to a specific (i.e., string in this case)
  type. I use it in instances when x is already a string, so the runtime system
  shouldn't have to do anything.*)
let stringify (x : 'a) : string =
  assert (Obj.tag (Obj.repr x) = Obj.string_tag);
  Obj.magic x

let stringify_int (x : 'a) : string =
  assert (Obj.tag (Obj.repr x) = Obj.int_tag);
  string_of_int (Obj.magic x)
