let valid_words = Arg.read_arg "~/data/dictionary.txt"

(**[validate_letters letters] [Some list], a list of seven chars, or [None] if
   letters is not a string of 7 letters, separated by spaces or commas*)
let validate_letters letters =
  match letters with
  | _ -> raise (Failure "Unimplemented: validate_letters")

(** [solve letters] returns a list of valid scrabble words given letters. *)
let solve letters =
  match validate_letters with
  | _ -> raise (Failure "Unimplemented: solve")

let main =
  print_endline "Welcome to team LION's Scrabble Solver!";
  print_endline "Enter 7 letters. They may be separated by spaces or commas.";
  match read_line () with
  | exception End_of_file -> ()
  | letters -> solve letters
