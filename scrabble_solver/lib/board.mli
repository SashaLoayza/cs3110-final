type t
(** The board**)

type direction
(** Down or Right.*)

type bword
(** bword is the board word type, has the coordinate to put it on, the letter
    list, and the direction ha *)

val init : t
(** empty board*)

val place : t -> Letter.t option -> int -> int -> t
(** place [board] [letter] [row] [column] is [board] with [letter] placed on
    [board\[row\]\[column\]]. If a letter is already on that tile, that letter
    is replaced by [letter].*)

val remove : t -> int -> int -> t
(** [remove board row column] is the board with the letter at position
    [board\[row\]\[column\]] removed. If there is no letter at that position,
    the same board is returned.*)

val add_word : t -> bword -> t
