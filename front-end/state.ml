(*
   Supporting definitions and functions for the translation from Flick to the
   NaaSty intermediate language
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty
open Task_model

(*If forbid_shadowing then names can only be declared once. This could be overly
  restrictive for normal use. If not forbid_shadowing, then the intermediate
  language's names become handled in a hashcons-like way.*)
let forbid_shadowing = false

(*Should types and terms be treated differently -- can types be treated as
  terms? If no, then the "sizeof" operator becomes very awkward.*)
let term_type_separation = false

type identifier_kind =
  | Undetermined (*used for lookups*)
  | Value
  | Disjunct of type_value (*the type of the union of which the identifier
                             indicats a disjunct*)
  | Field of type_value (*the type of the record in which this field occurs*)
  | Function_Name (*details should be looked up in the dedicated field in the symbol table*)
  | Defined_Function_Name
  | Channel_Name
  | Map_Name
let string_of_identifier_kind ?summary_types:(summary_types : bool = false) = function
  | Undetermined -> "Undetermined"
  | Value -> "Value"
  | Disjunct tv ->
    "Disjunct (" ^ Crisp_syntax.type_value_to_string true false min_indentation tv ^ ")"
  | Field tv ->
    "Field (" ^ Crisp_syntax.type_value_to_string ~summary_types:summary_types
                  true false min_indentation tv ^ ")"
  | Function_Name -> "Function_Name"
  | Defined_Function_Name -> "Defined_Function_Name"
  | Channel_Name -> "Channel_Name"
  | Map_Name -> "Map_Name"

let ik_is_field = function
  | Field _ -> true
  | _ -> false

(*The function/process where a symbol was declared, and whether the symbol
  was a parameter there*)
type declaration_scope =
  {
    name : function_name;
    is_parameter : bool;
  }

type term_symbol_metadata =
  {
    declaration_scope : declaration_scope option;
    dependency_index : bool;

    source_type : type_value option;
    naasty_type : naasty_type option;
    identifier_kind : identifier_kind;

    (*If the symbol is a channel name, then here we keep the (unique)
      channel identifier it maps to. This mapping is defined by the task
      graph generation. The chan_id is used to work out the channel offset
      within a task. The channel offset identifies a channel in the ICL
      runtime.*)
    channel_id : Task_model.chan_id option
  }

type state =
  { pragma_inclusions : string list;
    type_declarations : (type_name * type_value * naasty_type option) list;
    next_symbol : identifier;
    type_symbols : (string * identifier * naasty_type option) list;
    term_symbols : (string * identifier * term_symbol_metadata) list;
    (* FIXME the name of this field should be improved: it's used for both
         functions and processes.
       bool indicates whether it's a function or a process.*)
    crisp_funs : (function_name * (bool * function_type)) list;

    (*FIXME perhaps a different state table should be used during translation,
            rather than have a single state table used for almost everything the
            compiler does.*)
    (*current_task and task_graph carry information using which we can
      resolve references to channels (in the resulting process topology)
      in a way that's understandable to the ICL runtime (i.e., as offsets in an
      array of channels).*)
    current_task : Task_model.task_id;
    task_graph : Task_model.task_graph;
(*TODO
- channel names partitioned into two arrays of input & output channels
- both arrays are local to a task.
- but is this info used in the topology, to wire channels up between tasks?
   (in that case, arrays would not be really local to the task, since their
    offsets matter outside them).
*)
  }

exception State_Exc of string * state option

(*The initial state consists of empty values for most things, except for
  the symbol table, which is initialised with interpreted functions.
  This initialisation gives us initial_next_symbol, initial_term_symbols, and
    calls Functions.export_funs to populate crisp_funs.
*)
let initial_state =
  (*Metadata used for interpreted function symbols*)
  let def_fn_term_symbol_metadata =
  {
    declaration_scope = None;
    dependency_index = false;
    source_type = None;
    naasty_type = None;
    identifier_kind = Defined_Function_Name;
    channel_id = None;
  } in
  (*Initialise next_symbol and term_symbols based on the available
    interpreted function symbols*)
  let (initial_next_symbol, initial_term_symbols) =
    List.fold_right (fun (fn, _) (i, term_symbols) ->
      (i + 1, (fn, i, def_fn_term_symbol_metadata) :: term_symbols))
      Functions.export_funs (1, []) in
  { pragma_inclusions = [];
    type_declarations = [];
    next_symbol = initial_next_symbol;
    type_symbols = [];
    term_symbols = initial_term_symbols;
    crisp_funs = Functions.export_funs;
    current_task = -1; (*FIXME create a constant, say Task_model.no_task*)
    task_graph =
      (*FIXME create a constant, say Task_model.empty_graph;
              or accept the graph (and current_task) as a parameter
              to initial_state*)
      { Task_model.tasks = [];};
  }

type scope =
  | Type
  | Term of identifier_kind
let scope_to_str scope =
  match scope with
  | Type -> "type"
  | Term ik -> "symbol (" ^ string_of_identifier_kind ik ^ ")"

let forget_label_in_identifier_kind (ik : identifier_kind) =
  match ik with
  | Undetermined
  | Value
  | Function_Name
  | Defined_Function_Name
  | Channel_Name
  | Map_Name -> ik
  | Disjunct tv ->
    Disjunct (forget_label tv)
  | Field tv ->
    Field (forget_label tv)

(*scope contains the query_kind*)
let check_identifier_kind scope result_kind =
  let result_kind = forget_label_in_identifier_kind result_kind in
  let query_kind =
    match scope with
    | Term ik -> forget_label_in_identifier_kind ik
    | _ ->
      raise (State_Exc
        ("check_identifier_kind: Expecting scope to be term. Instead found " ^
         scope_to_str scope, None)) in
  if result_kind = Undetermined then
    raise (State_Exc ("check_identifier_kind: Identifier kind for value record
    in symbol table cannot be undetermined" (*FIXME give more info*), None))
  else if query_kind <> Undetermined && query_kind <> result_kind then
    raise (State_Exc ("check_identifier_kind: Mismatching identifier kinds! Was expecting " ^
              string_of_identifier_kind query_kind ^ " but found " ^
              string_of_identifier_kind result_kind, None))
  else true

(*Flags are used to use this function to carry out the same query but with different
  nuances:
  - if filter_scope then we use the scope parameter as a filter, not as a check.
    that is, if we look for name "x" with scope "Value", then if
    filter_scope=false then if we find an "x" with a non-Value scope an exception
    is thrown; otherwise we use the scope as a filter, and look for an "x" that is
    a Value.
  - if only_checking then an exception isn't thrown if the scope is of the wrong kind.
  - if unexceptional then exceptions aren't thrown (unless scope is of the wrong kind),
      instead None is return.
*)
let lookup_term_data ?filter_scope:(filter_scope : bool = false)
      ?unexceptional:(unexceptional : bool = false)
      ?only_checking:(only_checking : bool = false)
      ?st_opt:(st_opt : state option = None)
      (scope : scope) (symbols : ('a * 'b * term_symbol_metadata) list)
      (id : 'a) : ('b * term_symbol_metadata) option =
  if only_checking && scope = Type then None
  else
    let failed : bool ref = ref false in
    let query_kind =
      match scope with
      | Term ik -> ik
      | _ -> raise (State_Exc ("Expecting scope to be term. Instead found " ^
                               scope_to_str scope, st_opt)) in
    (*NOTE would be more efficient if we used better data structures, rather
           than indexing ad hoc.*)
    let l' =
      List.filter (fun (x, y, md) ->
        let result_kind = forget_label_in_identifier_kind md.identifier_kind in
        let query_kind = forget_label_in_identifier_kind query_kind in
        let scope_check : bool =
          if x <> id then false
          else if result_kind = Undetermined then
            if unexceptional then
              begin
                failed := true;
                false
              end
            else
              raise (State_Exc ("Identifier kind in symbol table for '" ^
                                Debug.stringify x ^ " cannot be undetermined", st_opt))
          else if filter_scope then
            query_kind = Undetermined || query_kind = result_kind
          else if query_kind <> Undetermined && query_kind <> result_kind then
            if unexceptional then
              begin
                failed := true;
                false
              end
            else
              raise (State_Exc ("id '" ^ Debug.stringify id ^ "' : Mismatching identifier kinds! Was expecting " ^
                        string_of_identifier_kind query_kind ^ " but found " ^
                        string_of_identifier_kind result_kind, st_opt))
          else true in
        x = id && scope_check) symbols in
    if !failed then None
    else
      match l' with
      | [] -> None
      | [(_, y, md)] -> Some (y, md)
      | _ ->
        if unexceptional then None
        else
          let ys_s =
            List.map (fun (_, y, _) -> Debug.stringify y) l'
            |> String.concat ", " in
          raise (State_Exc ("Found multiple resolvants for symbol " ^
                            Debug.stringify id ^ ": " ^ ys_s ^ ".", st_opt))

(*For simplicity (and to defend against the possibility that identifiers and
  type identifiers occupy the same namespace) the lookup is made on both
  namespaces.
  It also checks the identifier kind info, to detect mistmatches between the
  query and the data, and to detect a malformed state (i.e., having an
  Undetermined identifier in the state).*)
let lookup (swapped : bool) (scope : scope) (symbols : ('a * 'b * term_symbol_metadata) list)
      ?st_opt:(st_opt : state option = None)
      (type_symbols : ('a * 'b * 'd) list)
      (id_to_str : 'a -> string) (res_to_str : 'b -> string)
      (id : 'a) : 'b option =
  let type_lookup l =
    let l' = List.map (fun (x, y, _) -> (x, y)) l in
    if not (List.mem_assoc id l') then
      None
    else Some (List.assoc id l') in
  let term_lookup l =
    match lookup_term_data ~st_opt ~only_checking:true scope symbols id with
    | None -> None
    | Some (idx, _) -> Some idx in
  let type_lookup = type_lookup type_symbols in
  let normal_lookup = term_lookup symbols in
  if term_type_separation && type_lookup <> None && normal_lookup <> None then
    raise (State_Exc ("Somehow the symbol " ^ id_to_str id ^
              " is being used for both a type and a non-type", st_opt))
  else match scope with
    | Type ->
      begin
        match normal_lookup with
        | None -> type_lookup
        | Some idx ->
          if not term_type_separation then
            type_lookup
          else
            raise (State_Exc ("Type symbol " ^ id_to_str id ^
                      " was used in term, getting idx " ^
                      res_to_str idx, st_opt))
      end
    | Term _ -> (*NOTE the identifier kind will be checked during the call to
                       normal_lookup*)
      begin
        match type_lookup with
        | None -> normal_lookup
        | Some idx ->
          if not term_type_separation then
            normal_lookup
          else
            raise (State_Exc ("Symbol " ^ id_to_str id ^
                      " was used in type, getting idx " ^
                      res_to_str idx, st_opt))
      end

(*Lookup functions for names and indices. Note that (string) names are used for
identifiers in the Crisp AST, but (numeric) indices are used in the NaaSty AST.*)
let lookup_name (scope : scope) (st : state) (id : string) : identifier option =
  lookup ~st_opt:(Some st) false scope st.term_symbols st.type_symbols (fun x -> x) string_of_int id
let lookup_id (scope : scope) (st : state) (id : identifier) : string option =
  lookup ~st_opt:(Some st) true scope (List.map swap_1_2 st.term_symbols)
    (List.map swap_1_2 st.type_symbols) string_of_int (fun x -> x) id

(*Updates the type associated with a symbol in the symbol table*)
let update_symbol_type (id : identifier) (ty : naasty_type)
      (scope : scope) (st : state) : state =
  let type_symbols_transf =
    List.map (fun (name, idx, ty_opt) ->
      if id = idx then
        match ty_opt with
        | None -> (name, idx, Some ty)
        | Some ty' ->
            if ty <> ty' then
              raise (State_Exc ("Different types associated with same symbol!",
                               Some st))
            (*Here we are being lenient, because we could fail even if we try to
              assign the same type to the same symbol twice.*)
            else (name, idx, ty_opt)
      else (name, idx, ty_opt)) in
  let term_symbols_transf =
    List.map (fun ((name, idx, metadata) as original) ->
      if id = idx then
        begin
        ignore(check_identifier_kind scope metadata.identifier_kind);
        let metadata' =
          match metadata.naasty_type with
          | None -> { metadata with naasty_type = Some ty }
          | Some ty' ->
              if ty <> ty' then
                raise (State_Exc ("Different types associated with same
                symbol!", Some st))
              (*Here we are being lenient, because we could fail even if we try to
                assign the same type to the same symbol twice.*)
              else metadata in
        (name, idx, metadata')
        end
      else original) in
  { st with
    type_symbols = type_symbols_transf st.type_symbols;
    term_symbols = term_symbols_transf st.term_symbols;
  }

let lookup_symbol_type (id : identifier)
      (scope : scope) (st : state) : naasty_type option =
  let type_symbol_lookup =
    List.fold_right (fun (_, idx, ty_opt) acc ->
      if id = idx then ty_opt
      else acc) in
  let term_symbol_lookup =
    List.fold_right (fun (_, idx, metadata) acc ->
      if id = idx then
        if check_identifier_kind scope metadata.identifier_kind then
          metadata.naasty_type
        else raise (State_Exc ("Failed kind-check", Some st))
      else acc) in
  match scope with
  | Term _ -> term_symbol_lookup st.term_symbols None
  | Type -> type_symbol_lookup st.type_symbols None

let lookup_function_type (st : state) (function_name : string) : (bool * function_type) option =
  if not (List.mem_assoc function_name st.crisp_funs) then
    None
  else
    Some (List.assoc function_name st.crisp_funs)

(*Check whether a symbol is a dependency index.
  This info can be used to restrict the usage or behaviour of that symbol
  in the target language -- for instance, by setting it to be constant.*)
let symbol_is_di (id : int) (st : state) : bool =
  match lookup_id (Term Value) st id with
  | None -> failwith "symbol_is_di: symbol not in table" (*FIXME give more info*)
  | Some x -> true 
