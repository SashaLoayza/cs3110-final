open OUnit2
open Scrabble_solver

let point_val_test (name : string) (c : Letter.t) (expected_output : int) : test
    =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (Letter.point_value c)

let letter_tests =
  [
    point_val_test "a is one point" (Letter.from_input 'a') 1;
    point_val_test "A is one point" (Letter.from_input 'A') 1;
    point_val_test "D is two points" (Letter.from_input 'D') 2;
    point_val_test "B is three points" (Letter.from_input 'B') 3;
    point_val_test "F is four points" (Letter.from_input 'F') 4;
    point_val_test "K is five points" (Letter.from_input 'K') 5;
    point_val_test "J is eight points" (Letter.from_input 'J') 8;
    point_val_test "Q is ten points" (Letter.from_input 'Q') 10;
  ]

(*[get_words_test] tests both create_hash and find_words as the respective
  output and input use type t. The idea of using both is if a hastable is
  created with a given list of words, then those words will be output by the
  find_words function.*)
let get_words_test (name : string) (t : Dictionary.t) (l : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (Dictionary.get_words t l)

let dictionary_tests =
  [
    get_words_test "1 key value pair"
      (Dictionary.create_hash [| "oneWord" |])
      "oneWord" [ "oneWord" ];
    get_words_test "1 key, 2 values find based  on first word"
      (Dictionary.create_hash [| "oneWord"; "Wordone" |])
      "oneWord" [ "Wordone"; "oneWord" ];
    get_words_test "1 key, 2 values, find based on second word"
      (Dictionary.create_hash [| "oneWord"; "Wordone" |])
      "Wordone" [ "Wordone"; "oneWord" ];
    get_words_test "2 words, but find key is not present"
      (Dictionary.create_hash [| "oneWord"; "Wordone" |])
      "no" [];
  ]

let hand_tests = []
let catword = Word.from_input (0, 0) Right "cat"

let add_word_test (name : string) (board : Board.t) (word : Word.t)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.board_to_string (Board.add_word board word))
    ~printer:Fun.id

let place_test (name : string) (board : Board.t) (letter : Letter.t option)
    (row : int) (column : int) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.board_to_string (Board.place board letter row column))
    ~printer:Fun.id

let board_tests =
  [
    place_test "bruh" Board.init (Letter.from_input_opt 'a') 0 10 "idc"
    (*add_word_test "adding cat to empty board" Board.init catword
      "c,a,t,_,_,_,_,_,_,_,_,_,_,_,_";*);
  ]

let main_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten
         [ letter_tests; dictionary_tests; hand_tests; board_tests; main_tests ]

let _ = run_test_tt_main suite
