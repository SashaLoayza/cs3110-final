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

let contains_word_test (name : string) (t : Dictionary.t) (l : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (Dictionary.contains_word t l)

(*copy and pasted from main.ml *)
let valid_words = Arg.read_arg "data/dictionary.txt"
let table = Dictionary.create_hash valid_words

let full_d_test (name : string) (htable : Dictionary.t) (l : string)
    (expected_output : bool) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (Dictionary.contains_word htable l)

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
    contains_word_test "2 words, and not present"
      (Dictionary.create_hash [| "oneWord"; "Wordone" |])
      "nejrtykuli;klo" false;
    contains_word_test "2 words, and present"
      (Dictionary.create_hash [| "oneWord"; "Wordone" |])
      "oneWord" true;
    contains_word_test "2 words, and present"
      (Dictionary.create_hash [| "oneWord"; "Wordone" |])
      "onWoerd" false;
    full_d_test "Ians word: abyssal" table "ABYSSAL" true;
    full_d_test "not a word" table "POG" false;
  ]

let hand_tests = []

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
    (Board.row_to_string (Board.place board letter row column) row)
    ~printer:Fun.id

let add_word_test_row (name : string) (board : Board.t) (word : Word.t)
    (row : int) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.row_to_string (Board.add_word board word) row)
    ~printer:Fun.id

let add_word_test_col (name : string) (board : Board.t) (word : Word.t)
    (col : int) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.col_to_string (Board.add_word board word) col)
    ~printer:Fun.id

let validate_board_tests (name : string) (board : Board.t) (word : Word.t)
    (d : Dictionary.t) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.validate_board (Board.add_word board word) word d)

let catword = Word.from_input (0, 0) Right "cat"
let batword = Word.from_input (0, 5) Right "baths"
let bigword = Word.from_input (0, 0) Right "abcdefghijklmno"
let catwordc = Word.from_input (0, 0) Down "cat"
let batwordc = Word.from_input (0, 5) Down "baths"
let bigwordc = Word.from_input (0, 0) Down "abcdefghijklmno"
let centerword = Word.from_input (8, 8) Right "center"

let board_tests =
  [
    place_test "place 0 1" Board.init
      (Letter.from_input_opt 'a')
      0 1 "|_||a||_||_||_||_||_||_||_||_||_||_||_||_||_|";
    place_test "place 3 9" Board.init
      (Letter.from_input_opt 'j')
      3 9 "|_||_||_||_||_||_||_||_||_||j||_||_||_||_||_|";
    place_test "place 8 0" Board.init
      (Letter.from_input_opt 'y')
      8 0 "|y||_||_||_||_||_||_||_||_||_||_||_||_||_||_|";
    place_test "place 14 14" Board.init
      (Letter.from_input_opt 'm')
      14 14 "|_||_||_||_||_||_||_||_||_||_||_||_||_||_||m|";
    place_test "place 0 0" Board.init
      (Letter.from_input_opt 'y')
      0 0 "|y||_||_||_||_||_||_||_||_||_||_||_||_||_||_|";
    add_word_test_row "adding cat to empty board" Board.init catword 0
      "|C||A||T||_||_||_||_||_||_||_||_||_||_||_||_|";
    add_word_test_row "adding baths to empty board" Board.init batword 0
      "|_||_||_||_||_||B||A||T||H||S||_||_||_||_||_|";
    add_word_test_row "adding 14 letterr to empty board" Board.init bigword 0
      "|A||B||C||D||E||F||G||H||I||J||K||L||M||N||O|";
    add_word_test_row "adding 14 letterr to empty board" Board.init centerword 8
      "|_||_||_||_||_||_||_||_||C||E||N||T||E||R||_|";
    add_word_test_col "adding cat to empty board" Board.init catwordc 0
      "|C||A||T||_||_||_||_||_||_||_||_||_||_||_||_|";
    add_word_test_col "adding baths to empty board" Board.init batwordc 5
      "|B||A||T||H||S||_||_||_||_||_||_||_||_||_||_|";
    add_word_test_col "adding 14 letterr to empty board" Board.init bigwordc 0
      "|A||B||C||D||E||F||G||H||I||J||K||L||M||N||O|";
    validate_board_tests "testing cat is a valid word" Board.init catword table
      true;
    validate_board_tests "testing center is a valid word" Board.init centerword
      table true;
  ]

let main_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten
         [ letter_tests; dictionary_tests; hand_tests; board_tests; main_tests ]

let _ = run_test_tt_main suite
