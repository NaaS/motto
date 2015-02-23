(*
   Collection of general functions used across the code base
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

let inter (mid : string) (ss : string list) =
  List.fold_right (fun x s ->
    if s = "" then x
    else x ^ mid ^ s) ss ""

let opt_string (prefix : string) (s : string option) (suffix : string) : string =
  match s with
  | None -> ""
  | Some s' -> prefix ^ s' ^ suffix

let replicate (s : string) (count : int) =
  assert (count > -1);
  let rec replicate' (acc : string) (i : int) =
    if i = 0 then acc
    else replicate' (acc ^ s) (i - 1)
  in
    replicate' "" count

let indn (indent : int) : string =
  replicate " " indent

let mk_block (indent : int) (f : int -> 'a -> string) (l : 'a list) : string =
  List.fold_right (fun x already ->
    already ^ f indent x) l ""
