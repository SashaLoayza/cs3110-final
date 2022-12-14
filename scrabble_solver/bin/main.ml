open Scrabble_solver
open Hand
open Letter
open Dictionary

let valid_words = Arg.read_arg "data/dictionary.txt"

(** [table] creates a hashtable according to the specs in dictionary.ml.*)
let dictionaryTable = create_hash valid_words

let state = State.initial_state

(**[validate_letters letters] [string list], a list of strings, raises exception
   if letters is not a string list with each index only having length of 1*)
let rec validate_letters letters =
  match letters with
  | [] -> []
  | h :: t ->
      if String.length h < 2 then
        let num = Char.code (Char.uppercase_ascii h.[0]) in
        if (num > 64 && num < 91) || num = 48 then h.[0] :: validate_letters t
        else
          raise
            (Failure
               "Did not meet preconditions for letter input (error code: VL1)")
      else
        raise
          (Failure
             "Did not meet preconditions for letter input (error code: VL2)")

(** [solve letters] returns a list of valid scrabble words given letters. *)
let solve letters =
  if List.length (validate_letters letters) > 1 then
    List.iter print_endline
      (word_list valid_words (from_char_list (validate_letters letters)))
    (* print_endline (List.nth (word_list valid_words (from_char_list
       (validate_letters letters))) 0)*)
  else
    raise
      (Failure "Did not meet preconditions for letter input (error code: SL1)")

let main () =
  print_endline "Welcome to team LION's Scrabble Solver!";
  print_endline "Enter up to 7 letters. They must be separated by spaces.";
  match read_line () with
  | exception End_of_file -> ()
  | letters ->
      solve
        (letters |> String.split_on_char ' ' |> List.filter (fun x -> x <> ""))

let help () =
  let command_descriptions =
    "add: \n\
     To add a word [w] to the board starting at row [r] and column [c] with \
     [direction] (down or right), type command 'add [w] [r] [c] [direction]'.\n\n\
     view: To view the current board's position, type 'view'\n\n\
     To see this help command, type 'help-setup' at any time.\n"
  in
  print_endline command_descriptions

let read_hand () =
  match read_line () with
  | exception End_of_file -> ()
  | letters ->
      solve
        (letters |> String.split_on_char ' ' |> List.filter (fun x -> x <> ""))

let read_command s (state : State.t) =
  let capTrim = String.uppercase_ascii (String.trim s) in
  let comm = Command.cmd_of_string capTrim in
  State.execute_cmd state comm dictionaryTable

(* State.execute_cmd state input_cmd *)
let rec command_loop (s : State.t) =
  match read_line () with
  | exception End_of_file -> command_loop s
  | cmd ->
      let newState =
        try read_command cmd s
        with _ ->
          print_endline "Please enter valid sytax and try again";
          command_loop s
      in
      command_loop newState

let main (state : State.t) =
  print_endline "Welcome to team LION's Scrabble Solver!\n";
  print_endline
    "Currently, the scrabble board is empty.\n\
     Use the following commands to set up the board to your desired \
     configuration:\n";
  help ();
  print_endline
    "To enter solving mode, type 'solve' at any time. Then, to go back to \
     setup mode, type 'setup'.";
  command_loop state;
  ()

(* Execute the solver engine. *)
let () = main state
