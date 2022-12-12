open Letter

type tile_value =
  | Normal
  | DoubleLetter
  | TripleLetter
  | DoubleWord
  | TripleWord

type tile = {
  letter : Letter.t option;
  point : tile_value;
}

type t = tile list list

let row1 =
  [
    { letter = None; point = TripleWord };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = DoubleLetter };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
  ]

let row2 =
  [
    { letter = None; point = Normal };
    { letter = None; point = DoubleWord };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = TripleLetter };
    { letter = None; point = Normal };
  ]

let row3 =
  [
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = DoubleWord };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = DoubleLetter };
  ]

let row4 =
  [
    { letter = None; point = DoubleLetter };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = DoubleWord };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
  ]

let row5 =
  [
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = DoubleWord };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
  ]

let row6 =
  [
    { letter = None; point = Normal };
    { letter = None; point = TripleLetter };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = TripleLetter };
    { letter = None; point = Normal };
  ]

let row7 =
  [
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = DoubleLetter };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = Normal };
    { letter = None; point = DoubleLetter };
  ]

let first_half =
  [ row1 @ [ { letter = None; point = TripleWord } ] @ List.rev row1 ]
  @ [ row2 @ [ { letter = None; point = Normal } ] @ List.rev row2 ]
  @ [ row3 @ [ { letter = None; point = Normal } ] @ List.rev row3 ]
  @ [ row4 @ [ { letter = None; point = DoubleLetter } ] @ List.rev row4 ]
  @ [ row5 @ [ { letter = None; point = Normal } ] @ List.rev row5 ]
  @ [ row6 @ [ { letter = None; point = Normal } ] @ List.rev row6 ]
  @ [ row7 @ [ { letter = None; point = Normal } ] @ List.rev row7 ]

let middle_row =
  [ row1 @ [ { letter = None; point = Normal } ] @ List.rev row1 ]

(* init initializes an empty board*)
let init = first_half @ middle_row @ List.rev first_half
let place_word (board : t) (word : Word.t) = failwith "unimplemented"

let rec sublist first last lst =
  match lst with
  | [] -> failwith "Empty Sublist"
  | h :: t ->
      let tail = if last <= 0 then [] else sublist (first - 1) (last - 1) t in
      if first > 0 then tail else h :: tail

(* get obtains the tile of the board *)
let get t row column : tile =
  let r = List.nth t row in
  List.nth r column

(** place_tile places a tile on the board*)
let place_tile (board : t) tile row column =
  if row > 14 || column > 14 || row < 0 || column < 0 then
    failwith "Unbound row or column, please enter values between 0 and 14"
  else
    let new_row =
      let initial = List.nth board row in
      sublist 0 column initial
      @ (tile :: sublist (column + 1) (List.length initial - 1) initial)
    in
    sublist 0 (row - 1) board
    @ (new_row :: sublist (row + 1) (List.length board - 1) board)

(** place letter on a tile*)
let place board letter row column =
  if row > 14 || column > 14 || row < 0 || column < 0 then
    failwith "Unbound row or column, please enter values between 0 and 14"
  else
    let new_tile = { (get board row column) with letter } in
    place_tile board new_tile row column

let rec add_word_horizontal (board : t) (word : Word.t) : t =
  let r, c = word.pos in
  match word.letter_list with
  | [] -> board
  | h :: t ->
      let newboard = place board h r c in
      add_word_horizontal newboard
        { word with pos = (r, c + 1); letter_list = t }

let rec add_word_vertical (board : t) (word : Word.t) : t =
  let r, c = word.pos in
  match word.letter_list with
  | [] -> board
  | h :: t ->
      let newboard = place board h r c in
      add_word_vertical newboard { word with pos = (r + 1, c); letter_list = t }

let add_word (board : t) (word : Word.t) : t =
  match word.direction with
  | Right -> add_word_horizontal board word
  | Down -> add_word_vertical board word

let remove board row column = place board None row column

let unbound_check (board : t) (word : Word.t) =
  let r, c = word.pos in
  if r > 14 || r < 0 || c > 14 || c < 0 then failwith "Unbound placement"
  else
    match word.direction with
    | Down ->
        if c + word.length > 14 then failwith "Unbound placement" else true
    | Right ->
        if r + word.length > 14 then failwith "Unbound placement" else true

let horizontal_placement_check (board : t) (word : Word.t) =
  let r, c = word.pos in
  let tile_list = sublist c (c + word.length - 1) (List.nth board r) in
  let check_list = List.filter (fun i -> i.letter = None) tile_list in
  List.length check_list == List.length tile_list

(*tail recursive loop to return sublist of tiles*)
let rec subcol_loop board c r rE acc =
  if r = rE then acc else subcol_loop board c (r + 1) rE (get board r c :: acc)

(* sublist_column is similar to sublist except this time the outputted list is
   the list of tiles in the place the word is supposed to be inputted on*)
let rec sublist_column (board : t) (word : Word.t) =
  let r, c = word.pos in
  subcol_loop board c r (r + word.length - 1) []

let vertical_placement_check (board : t) (word : Word.t) =
  let tile_list = sublist_column board word in
  let check_list = List.filter (fun i -> i.letter = None) tile_list in
  List.length check_list == List.length tile_list

let placement_check (board : t) (word : Word.t) =
  match word.direction with
  | Right -> horizontal_placement_check board word
  | Down -> vertical_placement_check board word

let validate_placement (board : t) (word : Word.t) =
  unbound_check board word && placement_check board word

(** let validate_words (board : t) (word : Word.t) = if board = init then true
    else false (* else branch isn't updated*)

    let validate_board (board : t) (word : Word.t) = validate_placement board
    word && validate_words board word **)

(*To String functions*)
let rec tile_to_letters (tList : tile list) =
  match tList with
  | [] -> []
  | h :: t -> h.letter :: tile_to_letters t

let rec letter_opt_ts lopt =
  match lopt with
  | [] -> ""
  | None :: t -> letter_opt_ts t ^ "|_|"
  | Some v :: t -> letter_opt_ts t ^ "|" ^ String.make 1 (char_value v) ^ "|"

let rec col_tile_list board c r acc =
  if r = 15 then acc else col_tile_list board c (r + 1) (get board r c :: acc)

let rec row_tile_list board r c acc =
  if c = 15 then acc else row_tile_list board r (c + 1) (get board r c :: acc)

let row_to_string (board : t) r =
  let tList = tile_to_letters (row_tile_list board r 0 []) in
  letter_opt_ts tList

let col_to_string (board : t) c =
  let tList = tile_to_letters (col_tile_list board c 0 []) in
  letter_opt_ts tList

let board_to_string board =
  "\n" ^ "    1  2  3  4  5  6  7  8  9  10 11 12 13 14 15" ^ "\n1  "
  ^ row_to_string board 0 ^ "\n2  " ^ row_to_string board 1 ^ "\n3  "
  ^ row_to_string board 2 ^ "\n4  " ^ row_to_string board 3 ^ "\n5  "
  ^ row_to_string board 4 ^ "\n6  " ^ row_to_string board 5 ^ "\n7  "
  ^ row_to_string board 6 ^ "\n8  " ^ row_to_string board 7 ^ "\n9  "
  ^ row_to_string board 8 ^ "\n10 " ^ row_to_string board 9 ^ "\n11 "
  ^ row_to_string board 10 ^ "\n12 " ^ row_to_string board 11 ^ "\n13 "
  ^ row_to_string board 12 ^ "\n14 " ^ row_to_string board 13 ^ "\n15 "
  ^ row_to_string board 14 ^ "\n"
