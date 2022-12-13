type t
(** The 15x15 scrabble board and the letters placed on it.*)

type tile
(** A single tile of the board, which is possibly occupied by a letter.*)

val init : t
(** init is the empty scrabble boardt*)

val get_row : t -> int -> tile list
(** [get_row board r] is the [r]th row of [board] as a [tile list], 0-indexed*)

val get_column : t -> int -> tile list
(** [get_column board c] is the [c]th row of [board] as a [tile list], 0-indexed*)

val place : t -> Letter.t option -> int -> int -> t
(** place [board] [letter] [row] [column] is [board] with [letter] placed on
    [board\[row\]\[column\]]. If a letter is already on that tile, that letter
    is replaced by [letter].*)

exception PlacementCollision
(** exception raised when placing a tile on a tile that is already occupied*)

val add_word : t -> Word.t -> Dictionary.t -> t
(** [add_word board word] returns [board] with [word] added onto it *)

val row_to_string : t -> int -> string
(** [row_to_string board r] returns the string representation of the [r]th row
    of [board]*)

val col_to_string : t -> int -> string
(** [col_to_string board c] returns the string representation of the [c]th
    column of [board]*)

val board_to_string : t -> string
(** [board_to_string board] returns the string representation of the entire
    board *)

val validate_board : t -> Word.t -> Dictionary.t -> bool
(** [validate_board board] returns true if all words on [board] are valid words
    in the scrabble dictionary*)

val pretty_board : t -> string
(** [board_to_string board] returns the string representation of the entire
    board with the axis for user convenience*)
