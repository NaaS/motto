(*
   Collection of general functions used across the code base
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

let inter (mid : string) (ss : string list) =
  List.fold_right (fun x s ->
    if s = "" then x
    else x ^ mid ^ s) ss ""
;;

