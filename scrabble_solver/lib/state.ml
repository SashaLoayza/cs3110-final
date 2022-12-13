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
  | Command.BoardClear board ->
      { hand = st.hand; board = Board.init; prev_command = cmd }
  | Command.HandMake letter_list ->
      {
        hand = Hand.from_letter_list letter_list;
        board = st.board;
        prev_command = cmd;
      }
  | Command.View ->
      print_endline (Board.pretty_board st.board);
      st
  | Command.Undo -> failwith ""
  | Command.Exit -> failwith ""
  | Command.Solve -> failwith ""
  | Command.Empty -> failwith "impossible"
