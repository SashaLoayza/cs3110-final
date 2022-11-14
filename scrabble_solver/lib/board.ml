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

let rec sublist first last lst =
  match lst with
  | [] -> failwith "Empty Sublist"
  | h :: t ->
      let tail = if last = 0 then [] else sublist (first - 1) (last - 1) t in
      if first > 0 then tail else h :: tail

let place letter row column t =
  if row > 15 || column > 15 || row < 1 || column < 1 then
    failwith "Unbound row or column, please enter values between 1 and 15"
  else
    let new_row =
      let initial = List.nth t row in
      sublist 0 (column - 1) initial
      @ (letter :: sublist (column + 1) (List.length initial - 1) initial)
    in
    sublist 0 (row - 1) t @ (new_row :: sublist (row + 1) (List.length t - 1) t)

let remove l row column t = place { l with letter = None } row column t
