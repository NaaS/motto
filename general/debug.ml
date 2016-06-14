(*
   Collection of functions used across the code base for debugging-related activities.
   Nik Sultana, Cambridge University Computer Lab, July 2015

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)


(*Format a list of strings nicely*)
let print_list indentation l =
  let sep = "\n" ^ indentation ^ "\t" in
  if l = [] then "(empty)"
  else sep ^ String.concat sep l

(*This is used to avoid committing to a specific (i.e., string in this case)
  type. I use it in instances when x is already a string, so the runtime system
  shouldn't have to do anything.*)
let stringify (x : 'a) : string =
  if Obj.tag (Obj.repr x) = Obj.string_tag then
    Obj.magic x
  else if Obj.tag (Obj.repr x) = Obj.int_tag then
    string_of_int (Obj.magic x)
  else failwith "stringify over unknown type"

type colour =
 | Black
 | Red
 | Green
 | Yellow
 | Blue
 | Magenta
 | Cyan
 | White

let int_of_colour = function
 | Black -> 0
 | Red -> 1
 | Green -> 2
 | Yellow -> 3
 | Blue -> 4
 | Magenta -> 5
 | Cyan -> 6
 | White -> 7

let ansi_prefix = "\027[3"
let encolour (c : colour) (s : string) : string =
  ansi_prefix ^ string_of_int (int_of_colour c) ^ "m" ^ s

let foreground_colour (c : colour) : unit =
  ansi_prefix ^ string_of_int (int_of_colour c) ^ "m"
  |> print_string

let reset_colour = "\027[0;22m"
