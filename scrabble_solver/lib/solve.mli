val combinations : char list -> char list array
(** An array of all combinations of a given list of characters. The output array
    will have duplicate combinations in the case that the input array has
    duplicates.*)

val word_list : Dictionary.t -> char list -> string list
(** [word_list dictionary char_list] is a list of valid words that can be made
    out of the letters in [char_list], using dictionary as the set of valid
    words.*)
