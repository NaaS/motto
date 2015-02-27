(*
   User-facing Flick compiler tool
   Nik Sultana, Cambridge University Computer Lab, February 2015
*)

open General
open Crisp_syntax
open Naasty
open Naasty_aux
open Translation
open State
open State_aux
open Serialisation
;;

type configuration =
  { source_file : string option;
    output_directory : string option;
    max_task_cost : int option;
    cost_function_file : string option }
type arg_params =
  OutputDir
;;

let cfg : configuration ref = ref {
  source_file = None;
  output_directory = None;
  max_task_cost = None;
  cost_function_file = None} in
let next_arg : arg_params option ref = ref None in
let arg_idx = ref 1 in

while !arg_idx < Array.length Sys.argv - 1 do
  match Sys.argv.(!arg_idx) with
  | "--max_task_cost" -> failwith "Unsupported feature" (*TODO*)
  | "--cost_function_file" -> failwith "Unsupported feature" (*TODO*)
  | "-o" ->
    if !next_arg <> None then
      failwith ("Was expecting a parameter value before " ^ Sys.argv.(!arg_idx))
    else next_arg := Some OutputDir
  | s ->
    match !next_arg with
    | None ->
      cfg := { !cfg with source_file = Some s }
    | Some OutputDir ->
      cfg := { !cfg with output_directory = Some s }
done;

match !cfg.source_file, !cfg.output_directory with
| Some source_file, Some output_directory ->
  Crisp_parse.parse source_file
  |> Translation.naasty_of_flick_program

| _ ->
  begin
    failwith "Output directory and input file need to be specified";
  end
