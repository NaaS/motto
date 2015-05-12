(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, May 2015
*)

type configuration =
  { source_file : string option;
    output_directory : string option;
    max_task_cost : int option;
    cost_function_file : string option;
    (*Include directories are ordered by priority in which they are searched;
      this is in the reverse order they are provided on the command line.
      i.e., -I searched_dir_2 -I searched_dir_1*)
    include_directories : string list;
    disable_inlining : bool;
  }

let cfg : configuration ref = ref {
  source_file = None;
  output_directory = None;
  max_task_cost = None;
  cost_function_file = None;
  include_directories = [];
  disable_inlining = false;
}
