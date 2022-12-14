open Scrabble_solver
open Hand
open Letter
open Dictionary

let valid_words = Arg.read_arg "data/dictionary.txt"

(** [table] creates a hashtable according to the specs in dictionary.ml.*)
let dictionaryTable = Dictionary.create_hash valid_words

let state = State.initial_state

let read_command s (state : State.t) =
  let capTrim = String.uppercase_ascii (String.trim s) in
  let comm = Command.cmd_of_string capTrim in
  State.execute_cmd state comm dictionaryTable

(* State.execute_cmd state input_cmd *)
let rec command_loop (s : State.t) : unit =
  match read_line () with
  | exception End_of_file -> command_loop s
  | cmd ->
      let newState =
        try read_command cmd s
        with _ ->
          print_endline "Please enter valid sytax and try again";
          s
      in
      command_loop newState

let main (state : State.t) =
  print_endline "Welcome to team LION's Scrabble Solver!\n";
  print_endline
    "Currently, the scrabble board is empty.\n\
     Use the following commands to set up the board to your desired \
     configuration:\n";
  Command.help ();
  print_endline
    "To enter solving mode, type 'solve [r] [c]' at any time. Solving mode \
     will find possible words that use the letter at [r] [c].";
  command_loop state;
  ()

(* Execute the solver engine. *)
let () = main state
