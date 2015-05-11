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

(*Fold-right that threads another parameter through the fold, and behaving like
  a map. I use this for threading a state value through a "map".*)
let fold_map (z : 'b list * 'c) (f : 'c -> 'a -> 'b * 'c)
      (l : 'a list) : ('b list * 'c) =
  List.fold_right (fun ty (tys_acc, st_acc) ->
    let (ty', st_acc') = f st_acc ty
    in (ty' :: tys_acc, st_acc')) l z

(*Fold-right that threads another parameter through the fold. I use this for
  threading a state value.*)
(*FIXME still not happy with the name of this function!*)
let fold_couple (z : 'b * 'c) (f : 'c -> 'a -> 'b -> 'b * 'c)
      (l : 'a list) : ('b * 'c) =
  List.fold_right (fun ty (tys_acc, st_acc) ->
    let (tys_acc', st_acc') = f st_acc ty tys_acc
    in (tys_acc', st_acc')) l z

let swap (x, y) = (y, x)
let swap_1_2 (x, y, z) = (y, x, z)

let the_single l =
  match l with
  | [x] -> x
  | _ -> failwith "Tried to treat non-singleton list as a singleton."

let the x_opt =
  match x_opt with
  | Some x -> x
  | None -> failwith "Mistakenly assumed that an optional value contains a value."

let concat_pair (l : ('a list * 'b list) list) : 'a list * 'b list =
  List.fold_right (fun (item : 'a list * 'b list) (acc : 'a list * 'b list) ->
    (fst item @ fst acc, snd item @ snd acc)) l ([], [])

(*FIXME this currently simply prints files out; doesn't write them to the file
  system*)

type output_location = Stdout | Directory of string;;

let write_files dir (file_contents : (string * string) list) : unit =
  let output = 
    match dir with
      _ when Sys.file_exists dir && not (Sys.is_directory dir) -> print_endline (dir ^ " already exists, and is not a directory, printing to stdout") ; Stdout
    | _ when Sys.file_exists dir -> print_string ("directory " ^ dir ^ " already exists, overwrite files in it [Y/n]?");
                                    ( match (*input_line stdin*) read_line () with "Y"|"y"|"" -> Directory dir | _ -> Stdout )
    | _ -> print_endline "Creating output directory" ; Unix.mkdir dir 0o777 ; Directory dir;
  in
  List.fold_right (
      fun (filename, contents) _ -> match output with Stdout -> print_endline ("<<Starting " ^ filename);
								print_endline contents;
								print_endline (">>Finished " ^ filename)
						    | Directory dir -> let channel = open_out (dir ^ "/" ^ filename) in
								       output_string channel contents;
								       close_out channel;
    )
                  file_contents ()
