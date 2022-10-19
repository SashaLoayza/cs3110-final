(** A player's hand*)

type t

val from_char_list : char list -> t
val letter_list : t -> Letter.t list
val word_list : t -> string list
