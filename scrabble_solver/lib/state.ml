type t = {
  hand : Hand.t;
  board : Board.t;
  prev_command : Command.t;
}

(*state includes the last command*)
let initial_state =
  {
    hand = Hand.from_char_list [];
    board = Board.init;
    prev_command = Command.Empty;
  }

let execute_cmd st cmd dict =
  match cmd with
  | Command.BoardAddWord w ->
      {
        hand = st.hand;
        board = Board.add_word st.board w dict;
        prev_command = cmd;
      }
  | Command.BoardClear _ -> failwith ""
  | Command.HandMake _ -> failwith ""
  | Command.Undo -> failwith ""
  | Command.Solve -> failwith ""
  | Command.Empty -> failwith ""
  | _ -> failwith ""

(*Board.add_word board word dictionaryTable*)
