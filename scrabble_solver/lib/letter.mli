type t
(** Represents a letter *)

exception NotValidLetter of string
(** Rasied when an unkown letter is encountered. It carries the value not valid
    letter*)

val from_input : char -> t
(**[from_input j] is the in game letter representation of a character passed in
   as a *string*. Requires that j is a one letter string from the english
   alphabet.*)

val char_value : t -> char
(**[char_value l] is the character value of a letter. Requires that [l] is a
   valid representation of a letter.*)

val point_value : t -> int
(**[point_value l] is the point value of a letter. Requires that [l] is a valid
   representation of a letter*)
