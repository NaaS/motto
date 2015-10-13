(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, May 2015
*)

let version = "0.1"

type output_location = Stdout | Directory of string | No_output;;

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
                              run code generation or not. It's unset by default
                              at the moment. It gets set if the user provides
                              an -o argument from the command line.
                              In the future there may be multiple
                              backends, so this switch should turn into a
                              selector from multiple alternatives.*)
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
      disabled. This is useful to debugging and development.*)
    disable_data_model_checks : bool;
    (*Simplification involves evaluating expressions and commands at compile
      time, where possible. This transformation is done on the IL, not the
      source language.*)
    disable_simplification : bool;
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
  translate = false;
  summary_types = true;
  skip_type_check = false;
  unexceptional = false;
  run_compiled_runtime_script = false;
  dependency_valuation = [];
  front_end_and_state = false;
  naive_internal_naming = false;
  disable_data_model_checks = false;
  disable_simplification = false;
}
