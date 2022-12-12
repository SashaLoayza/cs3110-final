type direction =
  | Right
  | Down

type t = {
  pos : int * int;
  direction : direction;
  length : int;
  letter_list : Letter.t option list;
}

(*Take in string [word] and convert to a char list.*)
let string_to_char_list (word : string) : char list =
  List.init (String.length word) (fun n ->
      String.get word n |> Char.uppercase_ascii)

(*Create a word from input.*)
let from_input (pos : int * int) (direction : direction) (word : string) =
  let letter_list =
    word |> string_to_char_list |> List.map Letter.from_input_opt
  in
  { pos; direction; length = String.length word; letter_list }
