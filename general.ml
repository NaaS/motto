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

(*Some useful combinators for dataflow-style programming*)
let selfpair x = (x, x)
let apfst f (x, y) = (f x, y)
let apsnd f (x, y) = (x, f y)
let uncurry f (x, y) = f x y

(*Return the index of the first occurrence of an element in a list, or None.*)
let find_idx (l : 'a list) (x : 'a) : int option =
  List.fold_right (fun y (i, acc) ->
    match acc with
    | Some _ -> (i, acc)
    | None ->
      if x = y then (i, Some i)
      else (i + 1, None)) l (0, None)
  |> snd

(*This is used to avoid committing to a specific (i.e., string in this case)
  type. I use it in instances when x is already a string, so the runtime system
  shouldn't have to do anything.*)
let stringify (x : 'a) : string =
  assert (Obj.tag (Obj.repr x) = Obj.string_tag);
  Obj.magic x
