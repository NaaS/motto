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
    already ^ f indent x) (List.rev l) ""

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

(*Produce the list of all integers between from and until, including "from"
  and excluding "until". (If "from"="until" then an empty list is returned.)*)
let enlist (from : int) (until : int) : int list =
  assert (from <= until);
  let rec enlist' (acc : int list ) (cursor : int) =
    if cursor = until then List.rev acc
    else enlist' (cursor :: acc) (cursor + 1) in
  enlist' [] from
  |> List.rev

(*Like fold_map, but generates a tuple containing the various outputs, rather than
  a list. Its neater to use this function if we only have a few (e.g., 2)
  applications of f.*)
let (||>) ((x', st) : 'x * 'st) ((f, y) : ('st -> 'x -> ('x * 'st)) * 'x) : ('x * 'x * 'st) =
  let (y', st') = f st y in
  (x', y', st')

(*Split a list at the point where the given predicate is satisfied, if such a
  point exists.*)
let list_split (p : 'a -> bool) (l : 'a list) : 'a list * 'a option * 'a list =
  let rec list_split' (p : 'a -> bool) (l : 'a list) ((l1, x_opt, l2) : ('a list * 'a option * 'a list)) : 'a list * 'a option * 'a list =
    match l with
    | [] -> (List.rev l1, x_opt, List.rev l2)
    | y :: l' ->
      if p y then
        match x_opt with
        | None ->
          assert (l2 = []);
          list_split' p l' (l1, Some y, l2)
        | Some _ ->
          list_split' p l' (l1, x_opt, y :: l2)
      else
        match x_opt with
        | None ->
          assert (l2 = []);
          list_split' p l' (y :: l1, x_opt, l2)
        | Some _ ->
          list_split' p l' (l1, x_opt, y :: l2) in
  list_split' p l ([], None, [])

(*Split a list, or raise an exception if the list cannot be split*)
let list_split_exc (p : 'a -> bool) (l : 'a list) : 'a list * 'a * 'a list =
  match list_split p l with
  | (l1, Some x, l2) -> (l1, x, l2)
  | _ -> failwith "list_split_exc: predicate not matched"

(*Split a list at the nth item.
  (We start counting from 0)*)
let list_split_nth_exc (n : int) (l : 'a list) =
  assert (n < List.length l);
  let l' : ('a * int) list =
    enlist 0 (List.length l)
    |> List.rev
    |> List.combine l in
  let l1, (x, _), l2 = list_split_exc (fun (_, i) -> i = n) l' in
  (List.map fst l1), x, (List.map fst l2)

(*Add an item to an association list.
  If a v for k doesn't exist, then we simply add (k, v) to l;
  if a single mapping exists, then it is replaced with (k, v);
  if more than one mapping already exist, then an exception is raised.*)
(*FIXME this could be implemented more efficiently*)
let add_unique_assoc (((k,  _) as pair) : 'a * 'b) (l : ('a * 'b) list) : ('a * 'b) list =
  match List.filter (fun (k', _) -> k = k') l with
  | [] -> pair :: l
  | [(_, _)] ->
    List.fold_right (fun ((k', _) as pair') acc ->
      if k = k' then pair :: acc
      else pair' :: acc) (List.rev l) []
  | _ -> failwith "add_unique_assoc: found multiple entries for key"

(*Remove any elements in l1 from l2*)
let list_diff (l1 : 'a list) (l2 : 'a list) : 'a list =
  List.fold_right (fun x acc ->
    if List.mem x l1 then acc
    else x :: acc) l2 []
