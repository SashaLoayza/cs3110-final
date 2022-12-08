type t =
  (*state includes the last command*)
  | Initialize of {
      board : Board.t;
      prev_command : Command.t;
    }
    (*State when player is creating the board (populating it with letters/words
      for the first time.)*)
  | MakeHand of {
      hand : Hand.t;
      board : Board.t;
      prev_command : Command.t;
    }
    (*State when player is creating or changing the letters in their hand after
      initializing the board.*)
  | UpdateBoard of {
      hand : Hand.t;
      board : Board.t;
      prev_command : Command.t;
    }
    (*State when player is updating board after initializing the board and
      setting their hand.*)
  | Solve of {
      hand : Hand.t;
      board : Board.t;
      prev_command : Command.t;
    }
(*State when player is creating or changing the letters in their hand.*)

let execute_cmd t cmd = raise (Failure "Unimplemented: State.execute_cmd")
