(*
   Collection of general functions used across the code base
   Nik Sultana, Cambridge University Computer Lab, February 2015
   Jonny Shipton, Cambridge University Computer Lab, June 2016

   Use of this source code is governed by the Apache 2.0 license; see LICENSE
*)

let log m =
  print_endline (Printf.sprintf "\027[36m %s\027[m%!" m)

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

(*Weaker variant of List.assoc: it only works on assoc-lists where both elements
  have the same type, and if we don't find the key in the map, then we simply
  return the input unchanged*)
let apply_endomap (map : ('a * 'a) list) (x : 'a) : 'a =
  if List.mem_assoc x map then
    List.assoc x map
  else x

let repeat (n : int) (x : 'a) : 'a list =
  let rec repeat' (n : int) (x : 'a) (xs : 'a list) : 'a list =
    if n = 0 then xs
    else repeat' (n - 1) x (x :: xs)
  in
  assert (n >= 0);
  repeat' n x []

(*Referencing http://misc.flogisoft.com/bash/tip_colors_and_formatting*)
module Terminal = struct
  type terminal_colour =
    Default | Black | Red | Green | Yellow | Blue | Magenta | Cyan | LightGray
    | DarkGray | LightRed | LightGreen | LightYellow | LightBlue | LightMagenta
    | LightCyan | White
  type terminal_style = Bold | Dim | Underlined | Blink | Inverted | Hidden
  type terminal_formatting =
    { foreground:terminal_colour;
      background:terminal_colour;
      style:terminal_style }
  let foreground_colour_code = function
    | Default -> "39"
    | Black -> "30"
    | Red -> "31"
    | Green -> "31"
    | Yellow -> "33"
    | Blue -> "34"
    | Magenta -> "35"
    | Cyan -> "36"
    | LightGray -> "37"
    | DarkGray ->"90"
    | LightRed -> "91"
    | LightGreen -> "92"
    | LightYellow -> "93"
    | LightBlue -> "94"
    | LightMagenta -> "95"
    | LightCyan -> "96"
    | White -> "97"
  let background_colour_code = function
    | Default -> "49"
    | Black -> "40"
    | Red -> "41"
    | Green -> "42"
    | Yellow -> "43"
    | Blue -> "44"
    | Magenta -> "45"
    | Cyan -> "46"
    | LightGray -> "47"
    | DarkGray ->"100"
    | LightRed -> "101"
    | LightGreen -> "102"
    | LightYellow -> "103"
    | LightBlue -> "104"
    | LightMagenta -> "105"
    | LightCyan -> "106"
    | White -> "107"
  let style_code = function
    | Bold -> "1"
    | Dim -> "2"
    | Underlined -> "4"
    | Blink -> "5"
    | Inverted -> "7"
    | Hidden -> "8"
  let escape_char = "\027"
  let code_for codes = escape_char ^ "[" ^ String.concat ";" codes ^ "m"
  let reset_all = code_for ["0"]
  let colour colour text =
    code_for [foreground_colour_code colour] ^ text ^ reset_all
  let bold text =
    code_for [style_code Bold] ^ text ^ reset_all
  let format fmt text =
    code_for [foreground_colour_code fmt.foreground;
              background_colour_code fmt.background;
              style_code fmt.style] ^ text ^ reset_all
  let warning = format {foreground=Yellow; background=Default; style=Bold} "WARNING:"
  let error = format {foreground=Red; background=Default; style=Bold} "ERROR:"
end
