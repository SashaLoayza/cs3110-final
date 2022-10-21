let valid_words = Arg.read_arg "~/data/dictionary.txt"

(**[validate_letters letters] [string list], a list of seven strings, raises
   exception if letters is not a string list of length 7 with each inex only
   having at most length 1*)
let rec validate_letters letters =
  if List.length letters = 7 then
    match letters with
    | [] -> []
    | h :: t ->
        if String.length h < 2 then h.[0] :: validate_letters t
        else raise (Failure "Did not meet preconditions for letter input")
  else raise (Failure "Did not meet preconditions for letter input")

(** [solve letters] returns a list of valid scrabble words given letters. *)
let solve letters =
  match validate_letters letters with
  | _ -> raise (Failure "Unimplemented: solve")

let main =
  print_endline "Welcome to team LION's Scrabble Solver!";
  print_endline "Enter 7 letters. They must be separated by spaces.";
  match read_line () with
  | exception End_of_file -> ()
  | letters -> solve (String.split_on_char ' ' letters)
