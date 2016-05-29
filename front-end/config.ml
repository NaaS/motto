(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, May 2015
*)

open General

let version = "0.1"

type output_location = Stdout | Directory of string | No_output;;
let output_location_to_string = function
  | Stdout -> "stdout"
  | Directory s -> "Directory " ^ s
  | No_output -> "(No output)"
;;

type backend =
    (*A code generator will not be used, nor will IR be generated.*)
    No_backend
    (*Target the backend developed at ICL*)
  | Backend_ICL
    (*Target the OCaml-based Flick runtime system, but rather than
      using the "scriptable runtime" interface for interactively
      evaluating Flick programs, we generate OCaml code from Flick code,
      that will then call functions in the runtime's API.
      FIXME explore design choices:
        i) generate all code to run in a single instance of OCaml,
           using something like lwt for multithreading, and communicating
           via shared memory.
       ii) generate code to run in different instances of OCaml,
           scheduled by the OS, and communicating over IPC.
      iii) combination of schemes?
    *)
  | Backend_OCaml
let available_backends = [No_backend; Backend_ICL; Backend_OCaml];;
let backend_to_string = function
  | No_backend -> "no_backend"
  | Backend_ICL -> "ICL"
  | Backend_OCaml -> "OCaml"
;;

type configuration =
  { source_file : string option;
    output_location : output_location;
    max_task_cost : int option;
    cost_function_file : string option;
    (*Include directories are ordered by priority in which they are searched;
      this is in the reverse order they are provided on the command line.
      i.e., -I searched_dir_2 -I searched_dir_1*)
    include_directories : string list;
    (*Disable the inlining of intermediate variable introduced during the
      translation.*)
    disable_inlining : bool;
    (*Disable the erasure of declarations and assignments of unread variables*)
    disable_var_erasure : bool;
    verbosity : int;
    parser_test_files : string list;
    parser_test_dirs : string list;
    translate : bool; (*FIXME this is a crude flag indicating whether we want to
                              run code generation or not. It's set by default,
                              and currently cannot be changed from the
                              command-line. It's intended to help with
                              debugging, to dis/enable bits of the compiler,
                              but will probably eventually be removed since it's
                              no longer as useful as it used to be.*)
    (*If true, then summarise compound types (records and unions.*)
    summary_types : bool;
    (*If true, then we don't type (process and function) declarations after
      the program is parsed.*)
    skip_type_check : bool;
    (*Don't let exceptions float to the top, and don't report errors. instead
      simply output non-zero status code if there's an error, and zero otherwise.*)
    unexceptional : bool;
    run_compiled_runtime_script : bool;
    (*Mapping from dependency indices to values*)
    dependency_valuation : (string * int) list;
    (*Don't execute the back-end. Simply execute the front-end, then
      print the state.*)
    front_end_and_state : bool;
    (*When this flag is set, the names provided by the programmer are taken
      literally -- no attempt is made to rename to avoid collisions.
      A single global namespace is assumed, so the programmer needs to ensure
      that all names are unique.*)
    naive_internal_naming : bool;
    (*When this flag is set, checks built into the Data_model module are
      enable. This is useful to debugging and development. These checks are
      off by default since they are no longer up to date with how we currently
      use the data model.*)
    enable_data_model_checks : bool;
    (*Simplification involves evaluating expressions and commands at compile
      time, where possible. This transformation is done on the IL, not the
      source language.*)
    disable_simplification : bool;
    (*This flag effects the type inference algorithm. Parts of the algorithm can
      check an expression more deeply to see if it is well-typed. When this flag
      is set, those checks are disabled.*)
    default_nonstrict_type_checking : bool;
    (*Which backend to generate code for.*)
    backend : backend;
  }

let cfg : configuration ref = ref {
  source_file = None;
  output_location = Stdout;
  max_task_cost = None;
  cost_function_file = None;
  include_directories = [];
  disable_inlining = false;
  disable_var_erasure = false;
  verbosity = 0;
  parser_test_files = [];
  parser_test_dirs = [];
  translate = true;
  summary_types = true;
  skip_type_check = false;
  unexceptional = false;
  run_compiled_runtime_script = false;
  dependency_valuation = [];
  front_end_and_state = false;
  naive_internal_naming = false;
  enable_data_model_checks = false;
  disable_simplification = false;
  default_nonstrict_type_checking = false;
  backend = No_backend;
}

let configuration_to_string (cfg : configuration) : string list =
  ["source_file = " ^ bind_opt (fun s -> s) "n/a" cfg.source_file;
   "output_location = " ^ output_location_to_string cfg.output_location;
   "max_task_cost = " ^ bind_opt string_of_int "n/a" cfg.max_task_cost;
   "cost_function_file = " ^ bind_opt (fun s -> s) "n/a" cfg.cost_function_file;
   "include_directories = [" ^ String.concat ", " cfg.include_directories ^ "]";
   "disable_inlining  = " ^ string_of_bool cfg.disable_inlining;
   "disable_var_erasure = " ^ string_of_bool cfg.disable_var_erasure;
   "verbosity = " ^ string_of_int cfg.verbosity;
   "parser_test_files = [" ^ String.concat ", " cfg.parser_test_files ^ "]";
   "parser_test_dirs = [" ^ String.concat ", " cfg.parser_test_dirs ^ "]";
   "translate = " ^ string_of_bool cfg.translate;
   "summary_types = " ^ string_of_bool cfg.summary_types;
   "skip_type_check = " ^ string_of_bool cfg.skip_type_check;
   "unexceptional = " ^ string_of_bool cfg.unexceptional;
   "run_compiled_runtime_script = " ^ string_of_bool cfg.run_compiled_runtime_script;
   "dependency_valuation = [" ^ String.concat ", "
                                  (List.map (fun (s, i) ->
                                     "(" ^ s ^ "," ^ string_of_int i ^ ")")
                                     cfg.dependency_valuation) ^ "]";
   "front_end_and_state = " ^ string_of_bool cfg.front_end_and_state;
   "naive_internal_naming = " ^ string_of_bool cfg.naive_internal_naming;
   "enable_data_model_checks = " ^ string_of_bool cfg.enable_data_model_checks;
   "disable_simplification = " ^ string_of_bool cfg.disable_simplification;
   "default_nonstrict_type_checking = " ^ string_of_bool cfg.default_nonstrict_type_checking;
   "backend = " ^ backend_to_string cfg.backend;
  ]
