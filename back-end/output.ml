(*
   Handling output from the compiler
   Matteo Migliavacca, University of Kent, May 2015
   Nik Sultana, Cambridge University Computer Lab, May 2015
*)


open Config

let write_files (ol : output_location) (file_contents : (string * string) list) : unit =
  let output =
    match ol with
    | No_output
    | Stdout -> ol
    | Directory dir ->
      begin
        match dir with
          _ when Sys.file_exists dir && not (Sys.is_directory dir) ->
          print_endline (dir ^ " already exists, and is not a directory. Printing to stdout");
          Stdout
        | _ when Sys.file_exists dir ->
          begin
            print_string ("Directory " ^ dir ^
                          " already exists. Overwrite files in it [y/N]?");
            match read_line () with
              "Y" | "y" -> Directory dir
            | _ -> Stdout
          end
        | _ ->
          print_endline "Creating output directory";
          Unix.mkdir dir 0o777;
          Directory dir
      end
  in
  List.iter (fun (filename, contents) ->
    match output with
      Stdout ->
        print_endline ("<<Starting " ^ filename);
        print_endline contents;
        print_endline (">>Finished " ^ filename)
    | Directory dir ->
        let channel = open_out (dir ^ "/" ^ filename) in
        output_string channel contents;
        close_out channel
    | No_output -> ())
    file_contents
