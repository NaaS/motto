(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, May 2015
*)

type output_location = Stdout | Directory of string;;

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
    debug : bool;
  }

let cfg : configuration ref = ref {
  source_file = None;
  output_location = Stdout;
  max_task_cost = None;
  cost_function_file = None;
  include_directories = [];
  disable_inlining = false;
  disable_var_erasure = false;
  debug = false;
}
