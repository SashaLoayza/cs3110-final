type t =
  | BoardAddWord of {pos: int*int; direction : Board.direction; length : int; word: string}(*Add a word/succession of tiles to the board*)
  | BoardRemWord of {pos: int*int; direction : Board.direction; length : int}(*Remove a word/succession of tiles from the board*)
  | BoardClear (*Clear the board*)
  | HandMake of Letter.t list
    (* Clear the current hand, if any, and initialize a new hand of up to 7
       letters. *)
  | HandAdd of Letter.t
    (* Add a single letter to the current hand, if it has less than 7
       letters. *)
  | HandRem of Letter.t (* Remove a single letter from the current hand*)
  | Undo (* Undo the last command*)
  | Solve (* Run the solver engine and print the top solutions.*)
  | Exit  (* Exit the program*)

exception EmptyCommand
(**[parse_cmd cmd_string] is a list of strings, where the first string is the
   command passed in, and all subsequent strings are the arguments of the
   command (if any are given).Raises: an EmptyCommand exception, when the resulting list is empty. *)
let parse_cmd cmd_string : string list =
  let result = 
  List.filter (fun x -> x <> " ") (String.split_on_char ' ' cmd_string)in
  match result with 
  |[] -> raise EmptyCommand
  | _ -> result

let cmd_of_string string = raise (Failure "Unimplemented")
