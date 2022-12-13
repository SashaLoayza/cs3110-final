type t
(** Represents a single letter with a character and point value *)

exception NotValidLetter of string
(** Rasied when an unkown letter is encountered. It carries the value not valid
    letter*)

val from_input : char -> t
(**[from_input j] is the in game letter representation of a character passed in
   as a *char*. Requires that j is a one letter string from the english
   alphabet.*)

val from_input_opt : char -> t option
(**[from_input_opt j] is the in game letter opt representation of a character
   passed in as a *char*. Requires that j is a one letter string from the
   english alphabet or -.*)

val char_value : t -> char
(**[char_value l] is the character value of a letter. Requires that [l] is a
   valid representation of a letter.*)

val point_value : t -> int
(**[point_value l] is the point value of a letter. Requires that [l] is a valid
   representation of a letter*)

val make_let_opt_list : string -> t option list
(**[make_let_opt_list letters_string] is the [Letter.t option list] of letters
   in [letters_string], where the dash character is represented by [None] and
   regular character values are [Some letter]*)
