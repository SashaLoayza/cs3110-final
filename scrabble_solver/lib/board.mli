type t
(** The 15x15 scrabble board and the letters placed on it.*)

type tile
(**A single tile of the board, which is possibly occupied by a letter.*)

val get_row : t -> int -> tile list
(**[get_row board r] is the [r]th row of [board] as a [tile list], 0-indexed*)

val get_column : t -> int -> tile list
(**[get_column board c] is the [c]th row of [board] as a [tile list], 0-indexed*)

val init : t
(** empty board*)

val place : t -> Letter.t option -> int -> int -> t
(** place [board] [letter] [row] [column] is [board] with [letter] placed on
    [board\[row\]\[column\]]. If a letter is already on that tile, that letter
    is replaced by [letter].*)

val add_word : t -> Word.t -> t
val row_to_string : t -> int -> string
val col_to_string : t -> int -> string
val board_to_string : t -> string
val validate_board : t -> Word.t -> Dictionary.t -> bool