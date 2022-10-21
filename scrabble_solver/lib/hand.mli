(** A player's hand*)

type t

val from_char_list : char list -> t
(** [from_char_list char_list] is the representation of the players hand as a
    value of type t.*)

val letter_list : t -> Letter.t list
(** [letter_list t] is a [Letter.t list] of the letters in [t:hand]*)

val word_list : string array -> t -> string list
(** [word_list dictionary t] is a list of valid words that can be made out of
    the seven letters in [t:hand], using dictionary as the set of valid words.*)
