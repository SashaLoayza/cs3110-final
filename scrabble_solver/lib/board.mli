type t
(** The board **)

val place : Letter.t -> int -> int -> t
(* place [Letter.t] [int] [int] places a letter onto a board, if a letter is
   already on that tile, it replaces that letter*)

val remove : Letter.t -> int -> int -> t
(* remove [Letter.t] [int] [int] removes a tile on the board *)