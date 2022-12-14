open OUnit2
open Scrabble_solver
open Board

(** ################ TEST PLAN ################

    To test the scrabble_solver, we focused on ensuring the correctness of
    fundamental helper functions using black-box testing, exposing the types of
    data structures such as those in Hand.ml, Letter.ml, and Dictionary.ml. This
    allowed us to make sure that our concrete representations were storing the
    correct values (and satisfied our definition of the Abstraction Functions).

    Much of the program is manually tested, such as running the repl, since the
    main program has side effects (It prints to the screen) which cannot be
    (easily) tested directly. All of the command states were manually tested
    they have side effects based on inputs.

    We automatically test placing words and letters on the board. A major
    component of our test suite is supposed to mimic real world game play.This
    is seen in our additions of large words that have small overlaps. the tests
    validate each word addition and then further check to see if the board is
    created correctly.

    Using exceptions to view control structure helped us to test that our
    program was catching errors in the right places. As exceptions carry
    specific error messages, we were able to track the point of exception and
    conclude that our validations were catching user-input errors at the right
    point in the program.

    Modules Tested by OUnit:

    Board.ml

    - We test that placing a single tile on the board properly updates the board
      rep type
    - We test that adding words to the board properly updates the correct
      columns or rows
    - We test that validating the placement of correct boards returns true and
      visa versa

    Dictionary.ml

    - We test the Dictionary's hash table by adding all words from the
      dictionary.
    - We check that certain words are contained in the table, and others are not

    Letter.ml

    - We test that conversion between rep types of letters matches our
      abstractions
    - We test that letters have the correct point values

    Solve.ml

    - word_list_tests tests that expected valid words are produced by a given
      set of letters
    - Uses glass box testing by checking for membership of output using List.mem

    Word.ml

    - The Word.from_input function is tested in conjunction with board tests as
      the types are implicitly checked by OCaml's type system.

    Command.ml

    - Command_of_string is tested by checking the variant types of the output
    - We assert that bad commands raise failure (path through spec)

    State.ml is currently tested manually by the running of the program.

    The test plan should be located in a comment at the top of the test file.

    -4: The test plan is missing. -1: The test plan does not explain which parts
    of the system were automatically tested by OUnit vs. manually tested. -1:
    The test plan does not explain what modules were tested by OUnit and how
    test cases were developed (black box, glass box, randomized, etc.). -1: The
    test plan does not provide an argument for why the testing approach
    demonstrates the correctness of the system. *)
let char_val_test (name : string) (c : Letter.t) (expected_output : char) : test
    =
  name >:: fun _ -> assert_equal expected_output (Letter.char_value c)

let from_input_option (name : string) (c : char)
    (expected_output : Letter.t option) : test =
  name >:: fun _ -> assert_equal expected_output (Letter.from_input_opt c)

let point_val_test (name : string) (c : Letter.t) (expected_output : int) : test
    =
  name >:: fun _ -> assert_equal expected_output (Letter.point_value c)

let letter_tests =
  [
    char_val_test "a" (Letter.from_input 'a') 'A';
    char_val_test "B" (Letter.from_input 'B') 'B';
    char_val_test "z" (Letter.from_input 'z') 'Z';
    from_input_option "a" 'a' (Some (Letter.from_input 'a'));
    from_input_option "T" 'T' (Some (Letter.from_input 'T'));
    from_input_option "-" '-' None;
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

let word_list_test (name : string) (t : Dictionary.t) (char_list : char list)
    (contains_word : string) (expected_output : bool) : test =
  name >:: fun _ ->
  let found_words = Solve.word_list t char_list in
  assert_equal expected_output (List.mem contains_word found_words)

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

let combinations_test (name : string) (c : char list)
    (expected_output : char list array) : test =
  name >:: fun _ -> assert_equal expected_output (Solve.combinations c)

let combinations_len_test (name : string) (c : char list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Array.length (Solve.combinations c))

let word_list_tests =
  [
    word_list_test "HAT can be made with [A,H,T]" table [ 'a'; 'h'; 't' ] "HAT"
      true;
    word_list_test "THA cannot be made with [A,H,T]" table [ 'a'; 'h'; 't' ]
      "THA" false;
    word_list_test "BATTLE can be made with [A,B,T,T,E,L]" table
      [ 'a'; 'b'; 't'; 't'; 'e'; 'l' ]
      "BATTLE" true;
    word_list_test "BATTLE can be made with [M,A,B,T,T,E,L]" table
      [ 'm'; 'a'; 'b'; 't'; 't'; 'e'; 'l' ]
      "BATTLE" true;
    word_list_test "AA cannot be made with [M,A,B,T,T,E,L]" table
      [ 'm'; 'a'; 'b'; 't'; 't'; 'e'; 'l' ]
      "AA" false;
  ]

let combinations_tests =
  [
    combinations_test "testing a" [ 'a' ] [| [ 'a' ] |];
    (* combinations_test "testing a" [| 'a'; 'b' |] [| [ 'a' ]; [ 'b' ]; [ 'a';
       'b' ] |]; *)
    combinations_len_test "testing a length" [ 'a' ] 1;
    combinations_len_test "testing a, b" [ 'a'; 'b' ] 3;
    combinations_len_test "testing a, b, c" [ 'a'; 'b'; 'c' ] 7;
    combinations_len_test "testing a, b, c" [ 'a'; 'b'; 'c'; 'd' ] 15;
    combinations_len_test "combinations of 6 items"
      [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ]
      63;
    combinations_len_test "8 items"
      [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]
      255;
    combinations_len_test "12 items"
      [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l' ]
      4095;
    combinations_len_test "20 items"
      [
        'a';
        'b';
        'c';
        'd';
        'e';
        'f';
        'g';
        'h';
        'i';
        'j';
        'k';
        'l';
        'm';
        'n';
        'o';
        'p';
        'q';
        'r';
        's';
        't';
      ]
      1048575;
    combinations_len_test "20 items with repetitions"
      [
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
      ]
      1048575;
  ]

let add_words_test (name : string) (board : Board.t) (word : Word.t)
    (d : Dictionary.t) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.board_to_string (Board.add_word board word d))
    ~printer:Fun.id

let place_test (name : string) (board : Board.t) (letter : Letter.t option)
    (row : int) (column : int) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.row_to_string (Board.place board letter row column) row)
    ~printer:Fun.id

let add_word_test_row (name : string) (board : Board.t) (word : Word.t)
    (row : int) (d : Dictionary.t) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.row_to_string (Board.add_word board word d) row)
    ~printer:Fun.id

let add_word_test_col (name : string) (board : Board.t) (word : Word.t)
    (col : int) (d : Dictionary.t) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.col_to_string (Board.add_word board word d) col)
    ~printer:Fun.id

let validate_board_tests (name : string) (board : Board.t) (word : Word.t)
    (d : Dictionary.t) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.validate_board board word d)

let remove_test (name : string) (board : Board.t) (letter : Letter.t option)
    (row : int) (column : int) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.row_to_string
       (Board.remove (Board.place board letter row column) row column)
       row)
    ~printer:Fun.id

let catword = Word.from_input (7, 6) Right "cat"
let batword = Word.from_input (7, 5) Right "baths"
let bigword = Word.from_input (7, 0) Right "TARSOMETATARSUS"
let catwordc = Word.from_input (6, 7) Down "cat"
let batwordc = Word.from_input (5, 7) Down "baths"
let bigwordc = Word.from_input (0, 7) Down "TARSOMETATARSUS"
let centerword = Word.from_input (7, 7) Right "center"
let centerwordc = Word.from_input (7, 7) Down "center"
let aahword = Word.from_input (7, 7) Right "aah"
let aaword = Word.from_input (8, 7) Right "aa"

(* exception Collision of string *)

let colString = "Place word on an open spot"

let board_tests =
  [
    place_test "place 0 1" Board.init
      (Letter.from_input_opt 'a')
      0 1 "|_||A||_||_||_||_||_||_||_||_||_||_||_||_||_|";
    remove_test "place 0 1" Board.init
      (Letter.from_input_opt 'a')
      0 1 "|_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|";
    place_test "place 3 9" Board.init
      (Letter.from_input_opt 'j')
      3 9 "|_||_||_||_||_||_||_||_||_||J||_||_||_||_||_|";
    place_test "place 8 0" Board.init
      (Letter.from_input_opt 'y')
      8 0 "|Y||_||_||_||_||_||_||_||_||_||_||_||_||_||_|";
    place_test "place 14 14" Board.init
      (Letter.from_input_opt 'm')
      14 14 "|_||_||_||_||_||_||_||_||_||_||_||_||_||_||M|";
    place_test "place 0 0" Board.init
      (Letter.from_input_opt 'y')
      0 0 "|Y||_||_||_||_||_||_||_||_||_||_||_||_||_||_|";
    add_word_test_row "adding cat to empty board" Board.init catword 7 table
      "|_||_||_||_||_||_||C||A||T||_||_||_||_||_||_|";
    add_word_test_row "adding baths to empty board" Board.init batword 7 table
      "|_||_||_||_||_||B||A||T||H||S||_||_||_||_||_|";
    add_word_test_row "adding 14 letterr to empty board" Board.init bigword 7
      table "|T||A||R||S||O||M||E||T||A||T||A||R||S||U||S|";
    add_word_test_row "adding centered word to empty board" Board.init
      centerword 7 table "|_||_||_||_||_||_||_||C||E||N||T||E||R||_||_|";
    add_word_test_col "adding cat vertically to empty board" Board.init catwordc
      7 table "|_||_||_||_||_||_||C||A||T||_||_||_||_||_||_|";
    add_word_test_col "adding baths to empty board" Board.init batwordc 7 table
      "|_||_||_||_||_||B||A||T||H||S||_||_||_||_||_|";
    add_word_test_col "adding 14 letterr to empty board" Board.init bigwordc 7
      table "|T||A||R||S||O||M||E||T||A||T||A||R||S||U||S|";
    add_word_test_col "adding centered word to empty board" Board.init
      centerwordc 7 table "|_||_||_||_||_||_||_||C||E||N||T||E||R||_||_|";
    validate_board_tests "testing cat is a valid word" Board.init catword table
      true;
    validate_board_tests "testing center is a valid word" Board.init centerword
      table true;
    validate_board_tests "testing aah is a valid word" Board.init aahword table
      true;
    validate_board_tests "testing aa is a valid word after placing aah"
      (Board.add_word Board.init aahword table)
      aaword table true;
    validate_board_tests "testing cat is a valid word" Board.init catwordc table
      true;
    validate_board_tests "testing center is a valid word" Board.init centerwordc
      table true;
  ]

let deluxeword = Word.from_input (2, 8) Down "delux-"
let greatestword = Word.from_input (7, 3) Right "greatest"
let worldword = Word.from_input (5, 4) Down "wo-ld"
let editionword = Word.from_input (9, 3) Right "e-ition"
let teachword = Word.from_input (9, 6) Down "-each"
let dealersword = Word.from_input (2, 8) Right "-ealers"
let wammusword = Word.from_input (5, 4) Right "-amm-s"
let anaesthesiaword = Word.from_input (13, 0) Right "ANAEST-ESia"
let aDENOCARCINOMASword = Word.from_input (0, 0) Down "ADENOCARCINOM-S"

let big_words_test =
  [
    validate_board_tests "testing greatest is a valid word" Board.init
      greatestword table true;
    validate_board_tests "testing greatest and\n       deluxe is a valid board"
      (Board.add_word Board.init greatestword table)
      deluxeword table true;
    validate_board_tests
      "testing world and greatest\n       and deluxe is a validboard"
      (Board.add_word
         (Board.add_word Board.init greatestword table)
         deluxeword table)
      worldword table true;
    validate_board_tests
      "testing edition and world and greatest and deluxe\n\
      \       is a valid board"
      (Board.add_word
         (Board.add_word
            (Board.add_word Board.init greatestword table)
            deluxeword table)
         worldword table)
      editionword table true;
    validate_board_tests
      "testing teach and\n\
      \       edition\n\
      \  and world and\n\
      \  greatest and deluxe is a valid board"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word Board.init greatestword table)
               deluxeword table)
            worldword table)
         editionword table)
      teachword table true;
    add_words_test "adding a large\n       set of words"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word Board.init greatestword table)
               deluxeword table)
            worldword table)
         editionword table)
      teachword table
      "\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||D||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||E||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||L||_||_||_||_||_||_|\n\
       |_||_||_||_||W||_||_||_||U||_||_||_||_||_||_|\n\
       |_||_||_||_||O||_||_||_||X||_||_||_||_||_||_|\n\
       |_||_||_||G||R||E||A||T||E||S||T||_||_||_||_|\n\
       |_||_||_||_||L||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||E||D||I||T||I||O||N||_||_||_||_||_|\n\
       |_||_||_||_||_||_||E||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||A||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||C||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||H||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n";
    validate_board_tests
      "testing teach and edition\n\
      \  and world and\n\
      \  greatest and deluxe is\n\
      \       a valid board"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word Board.init greatestword table)
                  deluxeword table)
               worldword table)
            editionword table)
         teachword table)
      dealersword table true;
    add_words_test "adding a large set of words"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word Board.init greatestword table)
                  deluxeword table)
               worldword table)
            editionword table)
         teachword table)
      dealersword table
      "\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||D||E||A||L||E||R||S|\n\
       |_||_||_||_||_||_||_||_||E||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||L||_||_||_||_||_||_|\n\
       |_||_||_||_||W||_||_||_||U||_||_||_||_||_||_|\n\
       |_||_||_||_||O||_||_||_||X||_||_||_||_||_||_|\n\
       |_||_||_||G||R||E||A||T||E||S||T||_||_||_||_|\n\
       |_||_||_||_||L||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||E||D||I||T||I||O||N||_||_||_||_||_|\n\
       |_||_||_||_||_||_||E||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||A||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||C||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||H||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n";
    validate_board_tests
      "testing teach and edition\n\
      \  and world and\n\
      \  greatest and deluxe is\n\
      \       a valid board"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word
                     (Board.add_word Board.init greatestword table)
                     deluxeword table)
                  worldword table)
               editionword table)
            teachword table)
         dealersword table)
      wammusword table true;
    add_words_test "adding a\n       large set of words"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word
                     (Board.add_word Board.init greatestword table)
                     deluxeword table)
                  worldword table)
               editionword table)
            teachword table)
         dealersword table)
      wammusword table
      "\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||D||E||A||L||E||R||S|\n\
       |_||_||_||_||_||_||_||_||E||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||L||_||_||_||_||_||_|\n\
       |_||_||_||_||W||A||M||M||U||S||_||_||_||_||_|\n\
       |_||_||_||_||O||_||_||_||X||_||_||_||_||_||_|\n\
       |_||_||_||G||R||E||A||T||E||S||T||_||_||_||_|\n\
       |_||_||_||_||L||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||E||D||I||T||I||O||N||_||_||_||_||_|\n\
       |_||_||_||_||_||_||E||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||A||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||C||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||H||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n";
    validate_board_tests
      "testing teach and edition\n\
      \  and world and\n\
      \  greatest and deluxe is\n\
      \       a valid board"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word
                     (Board.add_word
                        (Board.add_word Board.init greatestword table)
                        deluxeword table)
                     worldword table)
                  editionword table)
               teachword table)
            dealersword table)
         wammusword table)
      anaesthesiaword table true;
    add_words_test "adding a large set of words"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word
                     (Board.add_word
                        (Board.add_word Board.init greatestword table)
                        deluxeword table)
                     worldword table)
                  editionword table)
               teachword table)
            dealersword table)
         wammusword table)
      anaesthesiaword table
      "\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||D||E||A||L||E||R||S|\n\
       |_||_||_||_||_||_||_||_||E||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||L||_||_||_||_||_||_|\n\
       |_||_||_||_||W||A||M||M||U||S||_||_||_||_||_|\n\
       |_||_||_||_||O||_||_||_||X||_||_||_||_||_||_|\n\
       |_||_||_||G||R||E||A||T||E||S||T||_||_||_||_|\n\
       |_||_||_||_||L||_||_||_||_||_||_||_||_||_||_|\n\
       |_||_||_||E||D||I||T||I||O||N||_||_||_||_||_|\n\
       |_||_||_||_||_||_||E||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||A||_||_||_||_||_||_||_||_|\n\
       |_||_||_||_||_||_||C||_||_||_||_||_||_||_||_|\n\
       |A||N||A||E||S||T||H||E||S||I||A||_||_||_||_|\n\
       |_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n";
    validate_board_tests
      "testing teach and edition\n\
      \  and world and\n\
      \  greatest and deluxe is\n\
      \       a valid board"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word
                     (Board.add_word
                        (Board.add_word
                           (Board.add_word Board.init greatestword table)
                           deluxeword table)
                        worldword table)
                     editionword table)
                  teachword table)
               dealersword table)
            wammusword table)
         anaesthesiaword table)
      aDENOCARCINOMASword table true;
    add_words_test "adding a large set of words"
      (Board.add_word
         (Board.add_word
            (Board.add_word
               (Board.add_word
                  (Board.add_word
                     (Board.add_word
                        (Board.add_word
                           (Board.add_word Board.init greatestword table)
                           deluxeword table)
                        worldword table)
                     editionword table)
                  teachword table)
               dealersword table)
            wammusword table)
         anaesthesiaword table)
      aDENOCARCINOMASword table
      "\n\
       |A||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |D||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n\
       |E||_||_||_||_||_||_||_||D||E||A||L||E||R||S|\n\
       |N||_||_||_||_||_||_||_||E||_||_||_||_||_||_|\n\
       |O||_||_||_||_||_||_||_||L||_||_||_||_||_||_|\n\
       |C||_||_||_||W||A||M||M||U||S||_||_||_||_||_|\n\
       |A||_||_||_||O||_||_||_||X||_||_||_||_||_||_|\n\
       |R||_||_||G||R||E||A||T||E||S||T||_||_||_||_|\n\
       |C||_||_||_||L||_||_||_||_||_||_||_||_||_||_|\n\
       |I||_||_||E||D||I||T||I||O||N||_||_||_||_||_|\n\
       |N||_||_||_||_||_||E||_||_||_||_||_||_||_||_|\n\
       |O||_||_||_||_||_||A||_||_||_||_||_||_||_||_|\n\
       |M||_||_||_||_||_||C||_||_||_||_||_||_||_||_|\n\
       |A||N||A||E||S||T||H||E||S||I||A||_||_||_||_|\n\
       |S||_||_||_||_||_||_||_||_||_||_||_||_||_||_|\n";
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           letter_tests;
           dictionary_tests;
           combinations_tests;
           board_tests;
           big_words_test;
           word_list_tests;
         ]

let _ = run_test_tt_main suite
