(** The direction of a word on the board, (only right and down are valid
    directions in scrabble)*)
type direction =
  | Right
  | Down

type t = {
  pos : int * int;
  direction : direction;
  length : int;
  letter_list : Letter.t option list;
}
(**A word that is to be placed on the board, where the letter_list is None at
   points where the player skips over an existing tile on the board. *)

(**Take in string [word] and convert to a char list.*)
let string_to_char_list (word : string) : char list =
  List.init (String.length word) (fun n ->
      String.get word n |> Char.uppercase_ascii)

(**Create a Word.t value from user input.*)
let from_input (pos : int * int) (direction : direction) (word : string) =
  let letter_list =
    word |> string_to_char_list |> List.map Letter.from_input_opt
  in
  { pos; direction; length = String.length word; letter_list }
