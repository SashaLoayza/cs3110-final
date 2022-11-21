type t
(** The type representing a state in the program's execution. The state
    encompasses the current state of the board and the current running
    environment (i.e. solve, )*)

val execute_cmd : t -> Command.t -> t
(** The new state after the given command has been executed.*)
