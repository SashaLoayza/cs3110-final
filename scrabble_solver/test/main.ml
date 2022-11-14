open OUnit2
open Scrabble_solver
open Hand
open Letter
open Dictionary

let point_val_test (name : string) (c : Letter.t) (expected_output : int) : test
    =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (point_value c)

let letter_tests =
  [
    point_val_test "a is one poiont" (from_input 'a') 1;
    point_val_test "A is one poiont" (from_input 'A') 1;
    point_val_test "D is one poiont" (from_input 'D') 2;
    point_val_test "B is one poiont" (from_input 'B') 3;
    point_val_test "F is one poiont" (from_input 'F') 4;
    point_val_test "K is one poiont" (from_input 'K') 5;
    point_val_test "J is one poiont" (from_input 'J') 8;
    point_val_test "Q is one poiont" (from_input 'Q') 10;
  ]

(*[get_words_test] tests both create_hash and find_words as the respective
  output and input use type t. The idea of using both is if a hastable is
  created with a given list of words, then those words will be output by the
  find_words function.*)
let get_words_test (name : string) (d : string array) (expected_output : int) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (point_value c)

let dictionary_tests = []
let hand_tests = []
let board_tests = []
let main_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten
         [ letter_tests; dictionary_tests; hand_tests; board_tests; main_tests ]

let _ = run_test_tt_main suite
