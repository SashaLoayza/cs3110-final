type t =
  | BoardAddWord of Word.t (*Add a word/succession of tiles to the board*)
  | BoardClear of Board.t (*Clear the board*)
  | HandMake of Letter.t list
    (* Clear the current hand, if any, and initialize a new hand of up to 7
       letters. *)
  | Undo (* Undo the last command*)
  | Solve (* Run the solver engine and print the top solutions.*)
  | Exit (* Exit the program*)
  | Empty (* Nothing *)

val cmd_of_string : string -> t
(**Given an input string, return the correct command of type t*)
