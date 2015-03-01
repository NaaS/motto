(*
   Generation of de/serialisers from Flick types
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty

(*Thrown when we try to generate a de/serialiser for a type that cannot be
  serialised -- either because of its nature (e.g., unit) or because it lacks
  annotations.*)
exception Unserialisable

type translated_type =
  { naasty_type : naasty_type;
    serialiser : naasty_function option;
    deserialiser : naasty_function option }

type data_model_component =
  { name : string;
    identifiers : string list;
    scheme : naasty_type }

(*
  Based on
   https://lsds.doc.ic.ac.uk/gitlab/naas/naas-box-system/blob/master/src/applications/hadoop_data_model/HadoopDMData.h 

  static HadoopDMData *bytes_stream_to_channel(char *stream, char *channel,
    char *streamend, size_t *bytes_read, size_t *bytes_written);
  static void write_bytes_to_channel(HadoopDMData *,char *chan,
    size_t *no_bytes);
  static void bytes_channel_to_stream(char *stream, char *channel,
    size_t *bytes_read, size_t *bytes_written);
  size_t get_channel_len();
  size_t get_stream_len();
*)
let get_channel_len =
  { name = "get_channel_len";
    identifiers = ["get_channel_len"];
    scheme = Fun_Type (-1, Size_Type None, [])}
let get_stream_len =
  { name = "get_stream_len";
    identifiers = ["get_stream_len"];
    scheme = Fun_Type (-1, Size_Type None, [])}
let bytes_stream_to_channel datatype =
  { name = "bytes_stream_to_channel";
    identifiers =
      ["bytes_stream_to_channel";
       datatype;
       "stream";
       "channel";
       "streamend";
       "bytes_read";
       "bytes_written"];
    scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Reference_Type (None,
                                             UserDefined_Type (None, -2))),
                [Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Char_Type None);
                 Reference_Type (Some (-5), Char_Type None);
                 Reference_Type (Some (-6), Size_Type None);
                 Reference_Type (Some (-7), Size_Type None)])}
let write_bytes_to_channel datatype =
  { name = "write_bytes_to_channel";
    identifiers =
      ["write_bytes_to_channel";
       datatype;
       "channel";
       "no_bytes"];
    scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Unit_Type),
                [Reference_Type (None, UserDefined_Type (None, -2));
                 Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Size_Type None)])}
let bytes_channel_to_stream =
  { name = "bytes_channel_to_stream";
    identifiers =
      ["bytes_channel_to_stream";
       "stream";
       "channel";
       "bytes_read";
       "bytes_written"];
    scheme =
      Fun_Type (-1,
                Static_Type (None,
                             Unit_Type),
                [Reference_Type (Some (-2), Char_Type None);
                 Reference_Type (Some (-3), Char_Type None);
                 Reference_Type (Some (-4), Size_Type None);
                 Reference_Type (Some (-5), Size_Type None)])}

(*Instantiates the data model for a particular serialisable datatype.
  This should generate part of the "struct" definition in the resulting C++
  translation of the Flick type. *)
let instantiate_data_model datatype =
  [get_channel_len; get_stream_len; bytes_stream_to_channel datatype;
   write_bytes_to_channel datatype; bytes_channel_to_stream]

(*
How this works:
- Given a Flick type we traverse it in the presented order (i.e., in the case of
   a record we proceed field by field in the given order)
- We look at each component of the type, starting with the leaves, and at the
   annotation of each component, to determine ????
- Variant's aren't supported: they're not used in our use-cases. Adding support
   for variants (at least for simple matching) isn't hard, it involves matching
   a token to determine which projection we're working in.
*)
let gen_serialiser (ty : type_value) : naasty_function =
  failwith "TODO"


let gen_deserialiser (ty : type_value) : naasty_function =
  failwith "TODO"

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

let rec expand_includes (p : Crisp_syntax.program) =
  List.fold_right (fun decl acc ->
    match decl with
    | Include source_file -> (*FIXME probably should search inside some set of
                                     include directories, given using -I
                                     parameter to the compiler.*)
      let inclusion = Crisp_parse.parse source_file
      in expand_includes inclusion @ acc
    | _ -> acc) p []

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
let translate_type_compilation_unit
      (types_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list =
  List.fold_right (fun (ty : Crisp_syntax.toplevel_decl) acc ->
    { Naasty_project.name = Crisp_syntax_aux.name_of_type ty;
      (*FIXME need to do this again, but for Cpp unit-type*)
      Naasty_project.unit_type = Naasty_project.Header;
      Naasty_project.inclusions = [];
      Naasty_project.content =
        (*FIXME discarding state information!*)
        fst (Translation.naasty_of_flick_program [ty])
    } :: acc)
    types_unit.Crisp_project.content []

(*FIXME currently ignoring functions and processes*)
let translate_serialise_stringify
      ((types_unit, functions_unit, processes_unit) :
         Crisp_project.compilation_unit *
         Crisp_project.compilation_unit *
         Crisp_project.compilation_unit) =
  let stringify_compilation_unit (cu : Naasty_project.compilation_unit) =
    (cu.Naasty_project.name, Naasty_project.string_of_compilationunit cu) in
  translate_type_compilation_unit types_unit
  |> List.map stringify_compilation_unit

;;
(*FIXME crude test*)
fold_map ([], State.initial_state) (fun st scheme ->
      Naasty_aux.instantiate true scheme.identifiers st scheme.scheme)
  (instantiate_data_model "test")
|> (fun (tys, st) ->
  let st_s = State_aux.state_to_str false st in
  let res_s = List.map (Naasty_aux.string_of_naasty_type ~st_opt:(Some st) 0) tys
    |> String.concat ";\n" in
  st_s ^ res_s)
|> print_endline
