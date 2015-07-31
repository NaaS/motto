(*
   Interface to runtime being developed at ICL.
   Nik Sultana, Cambridge University Computer Lab, June 2015
*)

open General
open State
open Naasty

module ICL_Backend : Backend.Instance =
struct
  type unit_name = string
  type unit_contents = string

(*Every type becomes 2 compilation units in NaaSty: a header file and a code
  file.*)
let translate_type_compilation_unit (st : state)
      (types_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_couple ([], st)
    (fun (st' : state) (decl : Crisp_syntax.toplevel_decl)
      (cunits : Naasty_project.compilation_unit list) ->
       let name = Crisp_syntax_aux.name_of_type decl in
       let (translated, st'') =
         if !Config.cfg.Config.translate then
           Translation.naasty_of_flick_program ~st:st' [decl]
         else ([], st') in
       let module Data_model_instance =
         Data_model.Instance(Data_model_consts.Values(
         struct
           let datatype_name = name
           let ty = Crisp_syntax_aux.the_ty_of_decl decl
         end)) in
       let (type_data_model_instance, st''') =
         fold_map ([], st'') (fun st scheme ->
           Naasty_aux.instantiate_type true scheme.Data_model.identifiers st scheme.Data_model.type_scheme)
           Data_model_instance.instantiate_data_model in
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
               type_data_model_instance]
         } in
       let (function_data_model_instance, st4) =
         fold_map ([], st''') (fun st scheme ->
           Naasty_aux.instantiate_function true scheme.Data_model.identifiers st
             scheme.Data_model.function_scheme)
           Data_model_instance.instantiate_data_model in
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
            List.map (fun fn -> Fun_Decl fn) function_data_model_instance
         }
       in (header_unit :: cpp_unit :: cunits, st4))
    types_unit.Crisp_project.content

let translate_function_compilation_unit (st : state)
      (functions_unit : Crisp_project.compilation_unit) :
  Naasty_project.compilation_unit list * state =
  fold_map ([], st)
    (fun (st' : state) (flick_f : Crisp_syntax.toplevel_decl) ->
       let name = "functions"(*FIXME extract name?*) in
       let (translated, st'') =
         if !Config.cfg.Config.translate then
           Translation.naasty_of_flick_program ~st:st' [flick_f]
         else ([], st') in
       ({Naasty_project.name = name;
         Naasty_project.unit_type = Naasty_project.Cpp;
         Naasty_project.inclusions = (*FIXME*)
           ["<stdlib.h>"];
         Naasty_project.content = translated
        }, (*FIXME generate header file together with this .cpp*)
        st''))
    functions_unit.Crisp_project.content

(*FIXME currently ignoring processes*)
let translate_serialise_stringify
  (st : State.state)
  ({Backend.types_unit; Backend.functions_unit; Backend.processes_unit} :
     Backend.compilation_record) =
  let stringify_compilation_unit (st : state) (cu : Naasty_project.compilation_unit) =
    (Naasty_project.filename_of_compilationunit cu,
     Naasty_project.string_of_compilationunit ~st_opt:(Some st) cu) in
  let (translated_type_units, st') =
    translate_type_compilation_unit st types_unit in
  let (translated_function_units, st'') =
    translate_function_compilation_unit st' functions_unit in
  if !Config.cfg.Config.debug then State_aux.state_to_str true st'' |> print_endline;
  List.map (stringify_compilation_unit st'')
    (translated_type_units @ translated_function_units)

let translate = translate_serialise_stringify

end

let translate st (tys, funs, procs) =
  ICL_Backend.translate st
    {Backend.types_unit = tys;
     Backend.functions_unit = funs;
     Backend.processes_unit = procs}
