type t
(** The board **)

type direction
(**Up, Down, Left, or Right.*)
val place : t -> Letter.t option -> int -> int -> t
(** place [board] [letter] [row] [column] is [board] with [letter] placed on
    [board\[row\]\[column\]]. If a letter is already on that tile, that letter
    is replaced by [letter].*)

val remove : t -> int -> int -> t
(** [remove board row column] is the board with the letter at position
    [board\[row\]\[column\]] removed. If there is no letter at that position,
    the same board is returned.*)


