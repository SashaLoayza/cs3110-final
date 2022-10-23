open Scrabble_solver
open Hand
open Letter

let valid_words = Arg.read_arg "data/dictionary.txt"

(**[validate_letters letters] [string list], a list of seven strings, raises
   exception if letters is not a string list of length 7 with each index only
   having at most length 1*)
let rec validate_letters letters =
  match letters with
  | [] -> []
  | h :: t ->
      if String.length h < 2 then h.[0] :: validate_letters t
      else raise (Failure "Did not meet preconditions for letter input 1")

(** [solve letters] returns a list of valid scrabble words given letters. *)
let solve letters =
  if List.length letters = 7 && List.length (validate_letters letters) > 1 then
    print_endline
      (List.nth
         (word_list valid_words (from_char_list (validate_letters letters)))
         0)
  else raise (Failure "Did not meet preconditions for letter input 2")

let main =
  print_endline "Welcome to team LION's Scrabble Solver!";
  print_endline "Enter 7 letters. They must be separated by spaces.";
  match read_line () with
  | exception End_of_file -> ()
  | letters ->
      solve
        (letters |> String.split_on_char ' ' |> List.filter (fun x -> x <> ""))
