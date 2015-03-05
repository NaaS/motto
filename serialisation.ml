(*
   Generation of de/serialisers from Flick types
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open State
open Crisp_syntax
open Naasty
open Data_model

(*Thrown when we try to generate a de/serialiser for a type that cannot be
  serialised -- either because of its nature (e.g., unit) or because it lacks
  annotations.*)
exception Unserialisable

type translated_type =
  { naasty_type : naasty_type;
    serialiser : naasty_function option;
    deserialiser : naasty_function option }

(*FIXME this kind of code seems beyond the scope of this module, so i'll
        probably move the code elsewhere later.*)
(*
   1. Given a parsed Flick program
   (1b. here would segment the process into possibly several processes, and
   specify which is the main one)
   2. form a project, splitting it into separate files, and keep track of
      inclusions
   3. translate each file into one or more naasty programs
      for type:
        traverse AST
        get type declaration
        attempt to translate it
        then generate datamodel for it, completing the struct definition
        then generate the code for the datamodel, completing the serialiser
   ?. need to keep track of names? in what way?
   4. generate task graph

   Handling exceptions:
   - arithmetic: overflow, division by zero
   - channels: breakages of channels
   - out of memory
*)

let expand_includes (p : Crisp_syntax.program) =
  let rec expand_includes' (p : Crisp_syntax.program) =
    List.fold_right (fun decl acc ->
      match decl with
      | Include source_file -> (*FIXME probably should search inside some set of
                                       include directories, given using -I
                                       parameter to the compiler.*)
        let inclusion =
          Crisp_parse.parse source_file
          |> List.rev
        in expand_includes' inclusion @ acc
      | _ -> decl :: acc) p []
  in List.rev (expand_includes' p)

(*Given a program whose Includes have been expanded out, separate out the
  declarations of types, processes, and functions -- but keep their relative
  order stable. That is, if we rewrite the program to contain the types, then
  the functions, then the processes, then all existing dependencies should
  continue to be satisified. (Originally, types, functions and processes can be
  given in any order as long as usual scoping rules are satisfied.)
  Then chop source file into different units, collecting declarations of the
  same kind*)
let split_declaration_kinds (p : Crisp_syntax.program) :
  Crisp_project.compilation_unit * Crisp_project.compilation_unit *
  Crisp_project.compilation_unit =
  List.fold_right (fun decl (types, functions, processes) ->
    match decl with
    | Type _ -> (decl :: types, functions, processes)
    | Process _ -> (types, functions, decl :: processes)
    | Function _ -> (types, decl :: functions, processes)
    | Include _ ->
      failwith "Inclusions should have been expanded before reaching this point.")
    p ([], [], [])
  |> (fun (types, functions, processes) ->
    ({ Crisp_project.name = "types";
       Crisp_project.content = List.rev types },
     { Crisp_project.name = "functions";
       Crisp_project.content = List.rev functions },
     { Crisp_project.name = "processes";
       Crisp_project.content = List.rev processes }))

(*Every type becomes 2 compilation units in NaaSty: a header file and a code
  file.*)
let translate_type_compilation_unit (st : state)
      (types_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_couple ([], st)
    (fun (st' : state) (ty : Crisp_syntax.toplevel_decl)
      (cunits : Naasty_project.compilation_unit list) ->
       let name = Crisp_syntax_aux.name_of_type ty in
       let (translated, st'') =
         Translation.naasty_of_flick_program ~st:st' [ty] in
       let (data_model_instance, st''') =
         fold_map ([], st'') (fun st scheme ->
           Naasty_aux.instantiate_type true scheme.identifiers st scheme.scheme)
           (instantiate_data_model name) in
       let header_unit =
         {Naasty_project.name = name;
          Naasty_project.unit_type = Naasty_project.Header;
          (*FIXME currently hardcoded, but this list of inclusions could be
                  extended based on an analysis of the code in the module.*)
          Naasty_project.inclusions =
            ["<stdint.h>";
             "<iostream>";
             "<assert.h>";
             "<exception>";
             "\"TaskBuffer.h\"";
             "\"applications/NaasData.h\""];
          Naasty_project.content =
            [Naasty_aux.add_fields_to_record (the_single translated)
               data_model_instance]
        } in
       let cpp_unit =
         {Naasty_project.name = name;
          Naasty_project.unit_type = Naasty_project.Cpp;
          (*FIXME currently hardcoded, but this list of inclusions could be
                  extended based on an analysis of the code in the module.*)
          Naasty_project.inclusions =
            ["\"" ^ Naasty_project.filename_of_compilationunit header_unit ^ "\"";
             "\"LinearBuffer.h\"";
             "<iostream>";
             "\"utils/ReadWriteData.h\"";
             "\"applications/NaasData.h\""];
          Naasty_project.content =
            [(*FIXME this needs to be instantiated from templates based on the
                     definition of the type in flick.*)]
         }
       in (header_unit :: cpp_unit :: cunits, st'''))
    types_unit.Crisp_project.content

let translate_function_compilation_unit (st : state)
      (functions_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_map ([], st)
    (fun (st' : state) (flick_f : Crisp_syntax.toplevel_decl) ->
       let name = "functions"(*FIXME extract name?*) in
       let (translated, st'') =
         Translation.naasty_of_flick_program ~st:st' [flick_f] in
       ({Naasty_project.name = name;
         Naasty_project.unit_type = Naasty_project.Cpp;
         Naasty_project.inclusions =
           [(*FIXME*)];
         Naasty_project.content = translated
        },
        st''))
    functions_unit.Crisp_project.content

(*FIXME currently ignoring processes*)
let translate_serialise_stringify
  (st : State.state)
  ((types_unit, functions_unit, processes_unit) :
     Crisp_project.compilation_unit *
     Crisp_project.compilation_unit *
     Crisp_project.compilation_unit) =
  let stringify_compilation_unit (st : state) (cu : Naasty_project.compilation_unit) =
    (Naasty_project.filename_of_compilationunit cu,
     Naasty_project.string_of_compilationunit ~st_opt:(Some st) cu) in
  let (translated_type_units, st') =
    translate_type_compilation_unit st types_unit in
  let (translated_function_units, st'') =
    translate_function_compilation_unit st' functions_unit in
  State_aux.state_to_str true st'' |> print_endline; (*FIXME debug line*)
  List.map (stringify_compilation_unit st'')
    (translated_type_units @ translated_function_units)

;;
(*FIXME crude test*)
fold_map ([], State.initial_state) (fun st scheme ->
      Naasty_aux.instantiate_type true scheme.identifiers st scheme.scheme)
  (instantiate_data_model "test")
|> (fun (tys, st) ->
  let st_s = State_aux.state_to_str true st in
  let res_s = List.map (Naasty_aux.string_of_naasty_type ~st_opt:(Some st) 0) tys
    |> String.concat ";\n" in
  st_s ^ res_s)
|> print_endline
