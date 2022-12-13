type t = {
  hand : Hand.t;
  board : Board.t;
  prev_command : Command.t;
}
(*state includes the last command*)

let execute_cmd t cmd = raise (Failure "Unimplemented: State.execute_cmd")

(*Board.add_word board word dictionaryTable*)