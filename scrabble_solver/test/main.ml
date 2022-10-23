open OUnit2
open Scrabble_solver
open Hand
open Letter

let point_val_test (name : string) (c : Letter.t) (expected_output : int) : test
    =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (point_value c)

let letter_tests = [ point_val_test "a is one poiont" (from_input 'a') 1 ]
let suite = "test suite for A2" >::: List.flatten [ letter_tests ]
let _ = run_test_tt_main suite
