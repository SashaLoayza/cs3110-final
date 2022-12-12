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

type direction =
  | Down
  | Right

type bword = {
  pos : int * int;
  direction : direction;
  length : int;
  letterlist : Letter.t option list;
}

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
let place_word (board : t) (word : bword) = failwith "unimplemented"

let rec sublist first last lst =
  match lst with
  | [] -> failwith "Empty Sublist"
  | h :: t ->
      let tail = if last = 0 then [] else sublist (first - 1) (last - 1) t in
      if first > 0 then tail else h :: tail

(* get obtains the tile of the board *)
let get t row column : tile =
  let r = List.nth t row in
  List.nth r column

let place_tile (board : t) tile row column =
  if row > 14 || column > 14 || row < 0 || column < 0 then
    failwith "Unbound row or column, please enter values between 0 and 14"
  else
    let new_row =
      let initial = List.nth board row in
      sublist 0 (column - 1) initial
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

let rec add_word_horizontal (board : t) (word : bword) : t =
  let r, c = word.pos in
  match word.letterlist with
  | [] -> board
  | h :: t ->
      let newboard = place board h (r - 1) (c - 1) in
      add_word_horizontal newboard
        { word with pos = (r, c + 1); letterlist = t }

let rec add_word_vertical (board : t) (word : bword) : t =
  let r, c = word.pos in
  match word.letterlist with
  | [] -> board
  | h :: t ->
      let newboard = place board h (r - 1) (c - 1) in
      add_word_vertical newboard { word with pos = (r + 1, c); letterlist = t }

let add_word (board : t) (word : bword) : t =
  match word.direction with
  | Right -> add_word_horizontal board word
  | Down -> add_word_vertical board word

let remove board row column = place board None row column

let unbound_check (board : t) (word : bword) =
  let r, c = word.pos in
  if r > 15 || r < 1 || c > 15 || c < 1 then failwith "Unbound placement"
  else
    match word.direction with
    | Down ->
        if c + word.length > 15 then failwith "Unbound placement" else true
    | Right ->
        if r + word.length > 15 then failwith "Unbound placement" else true

let horizontal_placement_check (board : t) (word : bword) =
  let r, c = word.pos in
  let tile_list =
    sublist (c - 1) (c + word.length - 2) (List.nth board (r - 1))
  in
  let check_list = List.filter (fun i -> i.letter = None) tile_list in
  List.length check_list == List.length tile_list

(*tail recursive loop to return sublist of tiles*)
let rec subcol_loop board c r rE acc =
  if r = rE then acc else subcol_loop board c (r + 1) rE (get board r c :: acc)

(* sublist_column is similar to sublist except this time the outputted list is
   the list of tiles in the place the word is supposed to be inputted on*)
let rec sublist_column (board : t) (word : bword) =
  let r, c = word.pos in
  subcol_loop board c r (r + word.length) []

(* im thinking you do sublist (c-1) (c-1) (List.nth board (r-1)) :: sublist
   (c-1) (c-1) (List.nth board (r)) :: sublist (c-1) (c-1) (List.nth board
   (r+1)) . . . until the length of that list equals the length of the word they
   inputted, this should work if my logic is correct but am unsure about
   implementing :( *)
let vertical_placement_check (board : t) (word : bword) = failwith "unimpl"

(* let tile_list = sublist_column board word in let check_list = List.filter
   (fun i -> i.letter = None) tile_list in List.length check_list == List.length
   tile_list *)
let placement_check (board : t) (word : bword) =
  match word.direction with
  | Right -> horizontal_placement_check board word
  | Down -> vertical_placement_check board word

let validate_placement (board : t) (word : bword) =
  unbound_check board word && placement_check board word

let validate_words (board : t) (word : bword) =
  if board = init then true else false (* else branch isn't updated*)

let validate_board (board : t) (word : bword) =
  validate_placement board word && validate_words board word

(*To String functions*)
let rec tile_to_letters (tList : tile list) =
  match tList with
  | [] -> []
  | h :: t -> h.letter :: tile_to_letters t

let rec letter_opt_ts lopt =
  match lopt with
  | [] -> ""
  | None :: t -> "," ^ letter_opt_ts t
  | Some v :: t -> String.make 1 (char_value v) ^ letter_opt_ts t

let rec col_tile_list board c r acc =
  if r = 14 then acc else col_tile_list board c (r + 1) (get board r c :: acc)

let rec row_tile_list board r c acc =
  if c = 14 then acc else row_tile_list board r (c + 1) (get board r c :: acc)

let row_to_string board r =
  let tList = tile_to_letters (row_tile_list board r 0 []) in
  letter_opt_ts tList

let col_to_string board c =
  let tList = tile_to_letters (col_tile_list board c 0 []) in
  letter_opt_ts tList
