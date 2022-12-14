val valid_words : string array
val dictionaryTable : Scrabble_solver.Dictionary.t
val state : Scrabble_solver.State.t
val help : unit -> unit
val read_command : string -> Scrabble_solver.State.t -> Scrabble_solver.State.t
val command_loop : Scrabble_solver.State.t -> Scrabble_solver.State.t
val main : Scrabble_solver.State.t -> unit
