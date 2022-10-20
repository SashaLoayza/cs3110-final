type t = {
  value : string;
  points : int;
}

exception NotValidLetter of string

(** [get_point_val s] is the point value of letter [s]. This is a helper
    function used by from_input. Requires that [s] is a valid english letter (1
    character) or the empty string*)
let get_point_val s =
  match s with
  | "A" | "E" | "I" | "O" | "U" | "L" | "N" | "S" | "T" | "R" -> 1
  | "D" | "G" -> 2
  | "B" | "C" | "M" | "P" -> 3
  | "F" | "H" | "V" | "W" | "Y" -> 4
  | "K" -> 5
  | "J" | "X" -> 8
  | "Q" | "Z" -> 10
  | "" -> 0
  | _ -> raise (NotValidLetter "Not a valid Letter")

let from_input s = { value = s; points = get_point_val s }
let char_value letter = letter.value
let point_value letter = letter.points
