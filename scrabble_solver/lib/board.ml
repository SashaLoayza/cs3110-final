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

exception PlacementCollision

let colString = "Place word on an open spot"
let get_row (board : t) r = List.nth board r

let get_column (board : t) c =
  List.fold_right (fun row_i acc -> List.nth row_i c :: acc) board []

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

(*auxillary sublist*)
let rec sublist2_aux first last lst index acc =
  if index >= first && index < last then
    sublist2_aux first last lst (index + 1) (List.nth lst index :: acc)
  else if index < last then sublist2_aux first last lst (index + 1) acc
  else acc

(**[sublist first last lst] is lst[first..last]. Requires: 0<= first <=
   length(lst) and first <= last <= list.length. (So List must be nonempty)
   Sublist: lower is inclusive and upper is exclusive*)
let sublist first last lst =
  if
    first >= 0
    && first <= List.length lst
    && last >= -1
    && last <= List.length lst
    && first <= last + 1
  then List.rev (sublist2_aux first last lst 0 [])
  else raise (Failure "Preconditions of sublist were not met")

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
      match (List.nth initial column).letter with
      | Some _ -> raise PlacementCollision
      | None ->
          sublist 0 column initial
          @ (tile :: sublist (column + 1) (List.length initial) initial)
    in
    let head_rows = if row = 0 then [] else sublist 0 row board in
    head_rows @ (new_row :: sublist (row + 1) (List.length board) board)

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
  | h :: t -> (
      match h with
      | Some v ->
          let newboard = place board h r c in
          add_word_horizontal newboard
            { word with pos = (r, c + 1); letter_list = t }
      | None ->
          add_word_horizontal board
            { word with pos = (r, c + 1); letter_list = t })

let rec add_word_vertical (board : t) (word : Word.t) : t =
  let r, c = word.pos in
  match word.letter_list with
  | [] -> board
  | h :: t -> (
      match h with
      | Some _ ->
          let newboard = place board h r c in
          add_word_vertical newboard
            { word with pos = (r + 1, c); letter_list = t }
      | None ->
          add_word_vertical board
            { word with pos = (r + 1, c); letter_list = t })

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

let validate_first (board : t) (word : Word.t) =
  let r, c = word.pos in
  match word.direction with
  | Right -> if r <> 7 || c > 7 then false else c + word.length - 1 >= 7
  | Down -> if c <> 7 || r > 7 then false else r + word.length - 1 <= 7

(*generate the coordinates in a list for a word that is horizontal*)
let rec right_pos lst (rs, cs) acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if h = "-" then right_pos t (rs, cs + 1) acc
      else right_pos t (rs, cs + 1) ((rs, cs) :: acc)

(*generate the coordinates in a list for a word that is vertical*)
let rec down_pos lst (rs, cs) acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if h = "-" then down_pos t (rs + 1, cs) acc
      else down_pos t (rs + 1, cs) ((rs, cs) :: acc)

(*Takes in a letter option list and returns a string list of each letter*)
let rec word_opt_ts lopt =
  match lopt with
  | [] -> []
  | None :: t -> String.make 1 ' ' :: word_opt_ts t
  | Some v :: t -> String.make 1 (char_value v) :: word_opt_ts t

(*Generates a list of the coordinates for each letter in word and does not
  generate coordinates for the dashes*)
let positions (word : Word.t) =
  let listOfLetters = List.rev (word_opt_ts word.letter_list) in
  match word.direction with
  | Right -> List.rev (right_pos listOfLetters word.pos [])
  | Down -> List.rev (down_pos listOfLetters word.pos [])

(*[none_smaller lst s] Scans lst to find the next value of a None that has a
  larger index than s*)
let rec none_smaller lst (s : int) : int =
  let temptile = List.nth lst s in
  match temptile.letter with
  | None -> s + 1
  | _ -> if s = 0 then s else none_smaller lst (s - 1)

(*[none_larger lst s] Scans lst to find the next value of a None that has a
  larger index than s *)
let rec none_larger lst (s : int) : int =
  let temptile = List.nth lst s in
  match temptile.letter with
  | None -> s - 1
  | _ -> if s = List.length lst - 1 then s else none_larger lst (s + 1)

(*given a coordinate, this function scans left to find the edge of the word.*)
let find_left board lst (r, c) = none_smaller lst c

(*given a coordinate, this function scans right to find the edge of the word.*)
let find_right board lst (r, c) = none_larger lst c

(*given a coordinate, this function scans up to find the edge of the word.*)
let find_up board lst (r, c) = none_smaller lst r

(*given a coordinate, this function scans down to find the edge of the word.*)
let find_down board lst (r, c) = none_larger lst r

(*converts a list of tiles into a list of letter options. Also used in to string
  methods*)
let rec tile_to_letters (tList : tile list) =
  match tList with
  | [] -> []
  | h :: t -> h.letter :: tile_to_letters t

(*get sublist of letters according to start and end indices of a list of tiles*)
let generate_word lst s e =
  List.fold_left
    (fun x y -> x ^ y)
    ""
    (sublist s (e + 1) (word_opt_ts (tile_to_letters lst)))

(*[horizontal_w b s] returns the start and end indices of the horizontal word
  that is connnected to s*)
let horizontal_w b (r, c) =
  let row = get_row b r in
  let sE = (find_left b row (r, c), find_right b row (r, c)) in
  generate_word row (fst sE) (snd sE)

(*[vertical_w b s] returns the start and end indices of the vertical word that
  is connnected to s*)
let vertical_w b (r, c) =
  let col = get_column b c in
  let sE = (find_up b col (r, c), find_down b col (r, c)) in
  generate_word col (fst sE) (snd sE)

(*validates the one large horizontal word for a horizontal word input*)
let validate_right_hor b cords d =
  let hword = horizontal_w b (List.nth cords 0) in
  if String.length hword < 2 then true else Dictionary.contains_word d hword

(*Validates all crosswords of the input word*)
let rec validate_right_verts b cords d =
  match cords with
  | [] -> true
  | h :: t ->
      let vword = vertical_w b h in
      if String.length vword < 2 then validate_right_verts b t d
      else Dictionary.contains_word d vword && validate_right_verts b t d

(*Validate the one large vertical word created when inputting a down facing
  word*)
let validate_down_vert b cords d =
  let vword = vertical_w b (List.nth cords 0) in
  if String.length vword < 2 then true else Dictionary.contains_word d vword

(*vlidates all of the corsswords of the vertical input word, ie checking all of
  the horizontal words that may or may not be created by the input word*)
let rec validate_down_hors b cords d =
  match cords with
  | [] -> true
  | h :: t ->
      let hword = horizontal_w b h in
      if String.length hword < 2 then validate_down_hors b t d
      else Dictionary.contains_word d hword && validate_down_hors b t d

let validate_words (board : t) (word : Word.t) (d : Dictionary.t) =
  let cords = positions word in
  match word.direction with
  | Right ->
      validate_right_hor board cords d && validate_right_verts board cords d
  | Down -> validate_down_vert board cords d && validate_down_hors board cords d

let validate_board (board : t) (word : Word.t) (d : Dictionary.t) =
  (*validate_placement board word &&*) validate_words board word d

(*******************to string**************************)
(*To String functions To string works for rows, columns and board*)

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
  "\n" ^ row_to_string board 0 ^ "\n" ^ row_to_string board 1 ^ "\n"
  ^ row_to_string board 2 ^ "\n" ^ row_to_string board 3 ^ "\n"
  ^ row_to_string board 4 ^ "\n" ^ row_to_string board 5 ^ "\n"
  ^ row_to_string board 6 ^ "\n" ^ row_to_string board 7 ^ "\n"
  ^ row_to_string board 8 ^ "\n" ^ row_to_string board 9 ^ "\n"
  ^ row_to_string board 10 ^ "\n" ^ row_to_string board 11 ^ "\n"
  ^ row_to_string board 12 ^ "\n" ^ row_to_string board 13 ^ "\n"
  ^ row_to_string board 14 ^ "\n"

let pretty_board board =
  "\n" ^ "    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14" ^ "\n0  "
  ^ row_to_string board 0 ^ "\n1  " ^ row_to_string board 1 ^ "\n2  "
  ^ row_to_string board 2 ^ "\n3  " ^ row_to_string board 3 ^ "\n4  "
  ^ row_to_string board 4 ^ "\n5  " ^ row_to_string board 5 ^ "\n6  "
  ^ row_to_string board 6 ^ "\n7  " ^ row_to_string board 7 ^ "\n8  "
  ^ row_to_string board 8 ^ "\n9  " ^ row_to_string board 9 ^ "\n10 "
  ^ row_to_string board 10 ^ "\n11 " ^ row_to_string board 11 ^ "\n12 "
  ^ row_to_string board 12 ^ "\n13 " ^ row_to_string board 13 ^ "\n14 "
  ^ row_to_string board 14 ^ "\n"
