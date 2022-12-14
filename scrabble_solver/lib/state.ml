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

(**[validate_letters letters] [string list], a list of strings, raises exception
   if letters is not a string list with each index only having length of 1*)
let rec validate_letters letters =
  match letters with
  | [] -> []
  | h :: t ->
      if String.length h < 2 then
        let num = Char.code (Char.uppercase_ascii h.[0]) in
        if (num > 64 && num < 91) || num = 48 then h.[0] :: validate_letters t
        else
          raise
            (Failure
               "Did not meet preconditions for letter input (error code: VL1)")
      else
        raise
          (Failure
             "Did not meet preconditions for letter input (error code: VL2)")

let array_of_list (lst : char list) : char array =
  failwith "convert a list to an array"

(** [solve letters] returns a list of valid scrabble words given letters. *)
let solve letters r c st dict =
  let charLetters = validate_letters letters in
  if List.length charLetters > 1 then
    let tileChar_opt = Board.get_letter_opt st.board r c in
    match tileChar_opt with
    | None -> failwith "Please solve on a letter already placed on the board"
    | Some v ->
        let tileChar = Letter.char_value v in
        let letters = tileChar :: charLetters in
        let pWords = Solve.word_list dict letters in
        let cWords = List.filter (fun x -> String.contains x tileChar) pWords in
        print_endline (List.fold_left (fun x y -> x ^ "\n" ^ y) "" cWords)
    (*List.iter print_endline (word_list valid_words (from_char_list
      (validate_letters letters))) print_endline (List.nth (word_list
      valid_words (from_char_list (validate_letters letters))) 0)*)
  else
    raise
      (Failure "Did not meet preconditions for letter input (error code: SL1)")

let main_solve (r : int) (c : int) st dict =
  print_endline "Welcome to team LION's Scrabble Solver!";
  print_endline "Enter up to 7 letters. They must be separated by spaces.";
  match read_line () with
  | exception End_of_file -> ()
  | letters ->
      solve
        (letters |> String.split_on_char ' ' |> List.filter (fun x -> x <> ""))
        r c st dict;
      ()

(* let read_hand () = match read_line () with | exception End_of_file -> () |
   letters -> solve (letters |> String.split_on_char ' ' |> List.filter (fun x
   -> x <> "")) *)

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
  | Command.Solve (r, c) ->
      main_solve r c st dict;
      st
  | Command.Empty -> failwith "impossible"
