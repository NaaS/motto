(*
   Collection of general functions used across the code base
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

let bind_opt (f : 'a -> 'b) (default : 'b) (x_opt : 'a option) : 'b =
  match x_opt with
  | None -> default
  | Some x -> f x

let opt_string (prefix : string) (s : string option) (suffix : string) : string =
  bind_opt (fun s' -> prefix ^ s' ^ suffix) "" s

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

(*Fold-right that threads another parameter through the fold. I use this for
  threading a state value.*)
let fold_map (z : 'b list * 'c) (f : 'c -> 'a -> 'b * 'c)
      (l : 'a list) : ('b list * 'c) =
  List.fold_right (fun ty (tys_acc, st_acc) ->
    let (ty', st_acc') = f st_acc ty
    in (ty' :: tys_acc, st_acc')) l z

let swap (x, y) = (y, x)

let the_single l =
  match l with
  | [x] -> x
  | _ -> failwith "Tried to treat non-singleton list as a singleton."

(*FIXME this currently simply prints files out; doesn't write them to the file
  system*)
let write_files (file_contents : (string * string) list) : unit =
  List.fold_right (fun (filename, contents) _ ->
    print_endline ("<<Starting " ^ filename);
    print_endline contents;
    print_endline (">>Finished " ^ filename))
    file_contents ()
