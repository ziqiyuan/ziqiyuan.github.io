open OUnit2
open Game
open Adventure
open Command
open State
open Change

let adventure_t = init_game

let all_levels_test name adv expected_output : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (all_levels adv)

let all_goals_test name adv expected_output : test =
  name >:: fun _ -> assert_equal expected_output (all_goals adv)

let adventure_tests =
  [
    all_levels_test "original fixed amount of levels" adventure_t 9;
    all_goals_test "original fixed goal points" adventure_t
      [|
        700; 2100; 4200; 5600; 7700; 9100; 11200; 12600; 14700; 16100;
      |];
  ]

(* Adventure functions are all tested. *)
let parse_test name str size expected_output : test =
  name >:: fun _ -> assert_equal expected_output (parse str size)

let command_tests =
  [
    ( "one index out of bound - positive" >:: fun _ ->
      assert_raises OutOfBound (fun () -> parse "1 9" 8) );
    ( "two indice out of bound -positive " >:: fun _ ->
      assert_raises OutOfBound (fun () -> parse "9 9" 8) );
    ( "one index out of bound - negative" >:: fun _ ->
      assert_raises OutOfBound (fun () -> parse "1 -9" 8) );
    ( "two indice out of bound - negative " >:: fun _ ->
      assert_raises OutOfBound (fun () -> parse "-9 -9" 8) );
    ( "one index out of bound - zero" >:: fun _ ->
      assert_raises OutOfBound (fun () -> parse "0 9" 8) );
    ( "two indice out of bound - zero " >:: fun _ ->
      assert_raises OutOfBound (fun () -> parse "0 0" 8) );
    ( "only one index present" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "5" 8) );
    ( "empty input" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "" 8) );
    ( "more than two digits" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "5 6 7" 8));
    ( "white space" >:: fun _ ->
      assert_raises Malformed (fun () -> parse " " 8));
    ("int expression" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "2+3 4" 8));
    ( "random string without white spaces" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "invalidinput" 7) );
    ( "random string with white space" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "HUman is" 7) );
    ( "random symbols with white space" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "@&*0" 7) );
    ( "quit Q capitalized" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "Quit" 7) );   
    parse_test "quit game" "quit" 3 Quit;
    parse_test "right bottom corner of 3*3" "3 3" 3 (Coor (2, 2));
    parse_test "right top corner of 3*3" "1 3" 3 (Coor (0, 2));
    parse_test "left top corner of 3*3" "1 1" 3 (Coor (0, 0));
    parse_test "left bottom corner 3*3" "3 1" 3 (Coor (2, 0));
    parse_test "random valid coordinate 10*10" "8 1" 10 (Coor (7, 0));
    parse_test "random valid coordinate 50*50" "48 35" 50
      (Coor (47, 34));
    parse_test "left top corner of 2*2" "1 1" 2 (Coor (0, 0));
    parse_test "left bottom corner of 2*2" "2 1" 2 (Coor (1, 0));
    parse_test "right top corner of 2*2" "1 2" 2 (Coor (0, 1));
    parse_test "right bottom corner of 2*2" "2 2" 2 (Coor (1, 1));
    parse_test "start with white spaces parse" " 2 3" 4 (Coor (1, 2));
    parse_test "extra blanks in the middle" "2    3" 4 (Coor (1, 2));
    parse_test "end with blanks" "4 6    " 8 (Coor (3, 5));
    parse_test "start and end with blanks" "   7   8   " 9 (Coor (6, 7));
  ]

let test_board_s14 = initiate 14
let test_board_s5 = initiate 5

let test_board_s20 = initiate 20

let test_board_s30 = initiate 30

let test_board_s40 = initiate 40

let test_board_s5_L1 = update_level test_board_s5

let test_board_s2 = initiate 2

let test_board_s50 = initiate 50

let current_size_test name st expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (current_size st) ~printer:string_of_int

let current_level_test name st expected_output : test =
  name >:: fun _ -> assert_equal expected_output (current_level st)

let live_median_board =
  [|
    [| 4; 0; 2; 1; 4 |];
    [| 0; 4; 0; 1; 0 |];
    [| 2; 0; 4; 3; 0 |];
    [| 2; 1; 1; 3; 4 |];
    [| 4; 3; 4; 3; 3 |];
  |]

let dead_median_board =
  [|
    [| 4; 0; 2; 1; 4 |];
    [| 0; 4; 0; 4; 0 |];
    [| 2; 0; 4; 3; 1 |];
    [| 4; 1; 2; 0; 4 |];
    [| 0; 3; 4; 3; 2 |];
  |]

let live_small_board = [| [| 4; 1 |]; [| 4; 0 |] |]

let dead_small_board = [| [| 4; 1 |]; [| 3; 0 |] |]

let dead_huge_board =
  [|
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
    [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3; 2; 3 |];
  |]

let live_huge_board =
  [|
    [| 3; 4; 0; 4; 2; 4; 2; 4; 4; 0; 0; 3; 3; 2; 4; 3; 2; 3; 0; 2 |];
    [| 3; 0; 0; 2; 0; 3; 3; 0; 0; 0; 1; 3; 0; 3; 1; 0; 0; 2; 2; 4 |];
    [| 4; 3; 1; 4; 2; 3; 1; 4; 2; 2; 0; 1; 3; 3; 0; 3; 4; 2; 3; 3 |];
    [| 3; 0; 0; 2; 0; 1; 2; 1; 2; 2; 3; 4; 0; 2; 2; 4; 1; 2; 0; 4 |];
    [| 2; 0; 1; 0; 0; 3; 4; 4; 0; 2; 0; 0; 3; 1; 2; 4; 1; 1; 1; 1 |];
    [| 2; 0; 0; 0; 4; 0; 4; 4; 2; 0; 2; 4; 3; 3; 0; 1; 2; 3; 1; 3 |];
    [| 2; 4; 3; 4; 4; 4; 4; 4; 3; 0; 4; 0; 2; 3; 0; 1; 3; 3; 2; 0 |];
    [| 3; 4; 1; 0; 1; 4; 0; 1; 4; 0; 1; 4; 1; 3; 1; 0; 0; 0; 3; 2 |];
    [| 2; 4; 3; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 4; 1; 3; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 1; 1; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 4; 0; 3; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1 |];
    [| 4; 0; 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4 |];
    [| 0; 4; 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2 |];
    [| 2; 4; 3; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 4; 1; 3; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 1; 1; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 4; 0; 3; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1 |];
    [| 4; 0; 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4 |];
    [| 0; 4; 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2 |];
  |]

let live_max_interface = 
  [|[|2; 3; 0; 2; 4; 0; 4; 3; 1; 3; 0; 3; 3; 3|];
    [|4; 0; 2; 0; 3; 0; 2; 3; 1; 4; 3; 0; 0; 2|];
    [|0; 4; 2; 1; 4; 2; 3; 2; 1; 1; 2; 4; 1; 3|];
    [|0; 4; 4; 0; 0; 4; 1; 2; 0; 3; 1; 0; 2; 4|];
    [|3; 3; 0; 3; 0; 4; 1; 1; 2; 4; 3; 2; 1; 3|];
    [|3; 2; 0; 3; 2; 0; 0; 3; 3; 2; 4; 0; 4; 4|];
    [|1; 0; 1; 1; 1; 0; 1; 0; 4; 2; 2; 0; 0; 0|];
    [|3; 0; 2; 0; 3; 4; 0; 1; 2; 4; 4; 0; 1; 2|];
    [|4; 0; 4; 4; 1; 4; 4; 4; 4; 1; 3; 2; 4; 2|];
    [|1; 1; 4; 0; 2; 2; 4; 3; 0; 0; 3; 3; 2; 0|];
    [|3; 2; 4; 3; 1; 1; 4; 4; 4; 2; 0; 2; 3; 3|];
    [|0; 4; 2; 2; 3; 2; 0; 2; 0; 1; 1; 0; 3; 2|];
    [|1; 0; 4; 3; 3; 1; 3; 3; 0; 2; 1; 0; 0; 4|];
    [|2; 4; 4; 4; 0; 1; 2; 1; 3; 0; 2; 1; 1; 2|]|]


let dead_max_interface = 
  [|[|1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0|];
    [|0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1|];
    [|1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0|];
    [|0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1|];
    [|1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0|];
    [|0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1|];
    [|1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0|];
    [|0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1|];
    [|1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0|];
    [|0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1|];
    [|1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0|];
    [|0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1|];
    [|1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0|];
    [|0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1|]|]

let negative_0 = [| [| 3; 4 |]; [| 4; 3 |] |]

let negative_1 =
  [|
    [| 1; 4; 0; 4; 0; 2 |];
    [| 3; 1; 0; 3; 2; 3 |];
    [| 2; 4; 2; 0; 0; 0 |];
    [| 0; 1; -1; 4; 3; 3 |];
    [| 4; 4; 1; 4; 3; 1 |];
    [| 0; 2; 4; 4; 2; 2 |];
  |]
    
let negative_n =
  [|
    [| -1; 4; 4; 3; 0; 3; 4; 1; 1 |];
    [| -1; 0; 0; 3; 0; 1; 0; 3; 2 |];
    [| -1; 0; 2; 4; 2; 1; 3; 3; 4 |];
    [| -1; 3; 3; 3; 0; 1; 3; 4; 0 |];
    [| 0; 1; 4; -1; -1; 4; 3; 2; 4 |];
    [| 4; 1; 1; -1; -1; 2; 3; 4; 3 |];
    [| 2; 3; 0; 2; 1; 1; 2; 3; 3 |];
    [| 0; 3; 3; 1; 0; 2; 4; 1; 3 |];
    [| 4; 1; 1; 1; 2; 1; 3; 2; 4 |];
  |]
    
let left_2 = [| [| 4; 2 |]; [| 4; 3 |] |]

let left_2_up = [| [| 2; -1 |]; [| 3; -1 |] |]

let down_2 = [| [| 4; 3 |]; [| 2; 2 |] |]

let down_2_up = [| [| -1; -1 |]; [| 4; 3 |] |]

let no_change_2 = [| [| 1; 2 |]; [| 4; 3 |] |]

let left_5 =
  [|
    [| 1; 1; 0; 3; 0 |];
    [| 2; 2; 0; 2; 4 |];
    [| 2; 3; 0; 1; 1 |];
    [| 4; 4; 0; 3; 1 |];
    [| 0; 2; 0; 2; 1 |];
  |]
    
let left_5_up =
  [|
    [| 1; 1; 3; 0; -1 |];
    [| 2; 2; 2; 4; -1 |];
    [| 2; 3; 1; 1; -1 |];
    [| 4; 4; 3; 1; -1 |];
    [| 0; 2; 2; 1; -1 |];
  |]

let down_5 =
  [|
    [| 0; 4; 3; 4; 3 |];
    [| 1; 1; 1; 3; 2 |];
    [| 1; 0; 1; 0; 1 |];
    [| 0; 0; 1; 1; 1 |];
    [| 4; 0; 1; 4; 0 |];
  |]

let down_5_up =
  [|
    [| -1; -1; -1; -1; -1 |];
    [| -1; 4; -1; 4; -1 |];
    [| 0; 0; -1; 3; 3 |];
    [| 0; 0; -1; 0; 2 |];
    [| 4; 0; 3; 4; 0 |];
  |]
    
let no_change_5 =
  [|
    [| 2; 3; 3; 3; 4 |];
    [| 4; 0; 2; 0; 0 |];
    [| 3; 1; 3; 2; 1 |];
    [| 4; 2; 1; 0; 3 |];
    [| 4; 2; 3; 3; 2 |];
  |]
    
let left_20 =
  [|
    [| 3; 3; 0; 4; 2; 4; 2; 4; 4; 0; 0; 3; 3; 2; 4; 3; 2; 3; 0; 2 |];
    [| 3; 3; 0; 2; 0; 3; 3; 0; 0; 0; 1; 3; 0; 3; 1; 0; 0; 2; 2; 4 |];
    [| 3; 3; 1; 4; 2; 3; 1; 4; 2; 2; 0; 1; 3; 3; 0; 3; 4; 2; 3; 3 |];
    [| 3; 3; 0; 2; 0; 1; 2; 1; 2; 2; 3; 4; 0; 2; 2; 4; 1; 2; 0; 4 |];
    [| 3; 3; 4; 0; 0; 3; 4; 4; 0; 2; 0; 0; 3; 1; 2; 4; 1; 1; 1; 1 |];
    [| 3; 3; 0; 0; 4; 0; 4; 4; 2; 0; 2; 4; 3; 3; 0; 1; 2; 3; 1; 3 |];
    [| 3; 3; 0; 4; 4; 4; 4; 4; 3; 0; 4; 0; 2; 3; 0; 1; 3; 3; 2; 0 |];
    [| 3; 3; 1; 0; 1; 4; 0; 1; 4; 0; 1; 4; 1; 3; 1; 0; 0; 0; 3; 2 |];
    [| 3; 3; 4; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 3; 3; 0; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 3; 3; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 3; 3; 0; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1 |];
    [| 3; 3; 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4 |];
    [| 3; 3; 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2 |];
    [| 3; 3; 2; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 3; 3; 1; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 3; 3; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 3; 3; 0; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1 |];
    [| 3; 3; 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4 |];
    [| 3; 3; 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2 |];
  |]
    
let left_20_up =
  [|
    [| 0; 4; 2; 4; 2; 4; 4; 0; 0; 3; 3; 2; 4; 3; 2; 3; 0; 2; -1; -1 |];
    [| 0; 2; 0; 3; 3; 0; 0; 0; 1; 3; 0; 3; 1; 0; 0; 2; 2; 4; -1; -1 |];
    [| 1; 4; 2; 3; 1; 4; 2; 2; 0; 1; 3; 3; 0; 3; 4; 2; 3; 3; -1; -1 |];
    [| 0; 2; 0; 1; 2; 1; 2; 2; 3; 4; 0; 2; 2; 4; 1; 2; 0; 4; -1; -1 |];
    [| 4; 0; 0; 3; 4; 4; 0; 2; 0; 0; 3; 1; 2; 4; 1; 1; 1; 1; -1; -1 |];
    [| 0; 0; 4; 0; 4; 4; 2; 0; 2; 4; 3; 3; 0; 1; 2; 3; 1; 3; -1; -1 |];
    [| 0; 4; 4; 4; 4; 4; 3; 0; 4; 0; 2; 3; 0; 1; 3; 3; 2; 0; -1; -1 |];
    [| 1; 0; 1; 4; 0; 1; 4; 0; 1; 4; 1; 3; 1; 0; 0; 0; 3; 2; -1; -1 |];
    [| 4; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1; -1; -1 |];
    [| 0; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0; -1; -1 |];
    [| 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3; -1; -1 |];
    [| 0; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1; -1; -1 |];
    [| 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4; -1; -1 |];
    [| 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2; -1; -1 |];
    [| 2; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1; -1; -1 |];
    [| 1; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0; -1; -1 |];
    [| 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3; -1; -1 |];
    [| 0; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1; -1; -1 |];
    [| 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4; -1; -1 |];
    [| 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2; -1; -1 |];
  |]
    
let down_20 =
  [|
    [| 3; 4; 0; 4; 2; 4; 2; 4; 4; 0; 0; 3; 3; 2; 4; 3; 2; 3; 0; 2 |];
    [| 3; 0; 0; 2; 0; 3; 3; 0; 0; 0; 1; 3; 0; 3; 1; 0; 0; 2; 2; 4 |];
    [| 4; 3; 1; 4; 2; 3; 1; 4; 2; 2; 0; 1; 3; 3; 0; 3; 4; 2; 3; 3 |];
    [| 3; 0; 0; 2; 0; 1; 2; 1; 2; 2; 3; 4; 0; 2; 2; 4; 1; 2; 0; 4 |];
    [| 2; 0; 1; 0; 0; 3; 4; 4; 0; 2; 0; 0; 3; 1; 2; 4; 1; 1; 1; 1 |];
    [| 2; 0; 0; 0; 4; 0; 4; 4; 2; 0; 2; 4; 3; 3; 0; 1; 2; 3; 1; 3 |];
    [| 2; 4; 3; 4; 4; 4; 4; 4; 3; 0; 4; 0; 2; 3; 0; 1; 3; 3; 2; 0 |];
    [| 3; 4; 1; 0; 1; 4; 0; 1; 4; 0; 1; 4; 1; 3; 1; 0; 0; 0; 3; 2 |];
    [| 2; 4; 3; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 4; 1; 3; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 1; 1; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 4; 0; 3; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1 |];
    [| 4; 0; 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4 |];
    [| 0; 4; 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2 |];
    [| 2; 4; 3; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 4; 1; 3; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 1; 1; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 3; 0; 3; 1; 2; 0; 1; 2; 3; 1; 3; 3; 2; 2; 3; 0; 2; 0; 0; 1 |];
    [| 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4 |];
    [| 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4 |];
  |]
    
let down_20_up =
  [|
    [|
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
    |];
    [|
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
      -1;
    |];
    [| 3; 4; 0; 4; 2; 4; 2; 4; 4; 0; 0; 3; 3; 2; 4; 3; 2; 3; 0; 2 |];
    [| 3; 0; 0; 2; 0; 3; 3; 0; 0; 0; 1; 3; 0; 3; 1; 0; 0; 2; 2; 4 |];
    [| 4; 3; 1; 4; 2; 3; 1; 4; 2; 2; 0; 1; 3; 3; 0; 3; 4; 2; 3; 3 |];
    [| 3; 0; 0; 2; 0; 1; 2; 1; 2; 2; 3; 4; 0; 2; 2; 4; 1; 2; 0; 4 |];
    [| 2; 0; 1; 0; 0; 3; 4; 4; 0; 2; 0; 0; 3; 1; 2; 4; 1; 1; 1; 1 |];
    [| 2; 0; 0; 0; 4; 0; 4; 4; 2; 0; 2; 4; 3; 3; 0; 1; 2; 3; 1; 3 |];
    [| 2; 4; 3; 4; 4; 4; 4; 4; 3; 0; 4; 0; 2; 3; 0; 1; 3; 3; 2; 0 |];
    [| 3; 4; 1; 0; 1; 4; 0; 1; 4; 0; 1; 4; 1; 3; 1; 0; 0; 0; 3; 2 |];
    [| 2; 4; 3; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 4; 1; 3; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 1; 1; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 4; 0; 3; 1; 2; 4; 1; 2; 3; 4; 3; 3; 2; 4; 4; 0; 2; 0; 4; 1 |];
    [| 4; 0; 1; 0; 3; 2; 1; 4; 3; 3; 1; 0; 4; 4; 3; 4; 4; 3; 4; 4 |];
    [| 0; 4; 0; 4; 4; 3; 4; 1; 2; 2; 4; 2; 1; 0; 1; 0; 1; 4; 0; 2 |];
    [| 2; 4; 3; 4; 1; 1; 4; 4; 0; 1; 1; 4; 2; 3; 3; 4; 0; 0; 1; 1 |];
    [| 4; 1; 3; 0; 0; 4; 2; 0; 4; 3; 2; 1; 0; 0; 1; 0; 0; 2; 3; 0 |];
    [| 1; 1; 2; 0; 4; 1; 2; 2; 0; 2; 3; 0; 3; 4; 2; 3; 3; 4; 4; 3 |];
    [| 3; 0; 3; 1; 2; 0; 1; 2; 3; 1; 3; 3; 2; 2; 3; 0; 2; 0; 0; 1 |];
  |]
    
let all_2 = [| [| 0; 0 |]; [| 0; 0 |] |]

let all_2_up = [| [| -1; -1 |]; [| -1; -1 |] |]


let live_board_test name board expected_output : test =
  name >:: fun _ -> assert_equal expected_output (live_board board)

let current_score_test name st expected_output : test =
  name >:: fun _ -> assert_equal expected_output (current_score st)

let rec mult f x n = if n = 1 then f x else mult f (f x) (n - 1)

let state_tests =
  [
    current_size_test "size-5 current_size" test_board_s5 5;
    current_size_test "size-2 current_size" test_board_s2 2;
    current_size_test "size-14 current_size" test_board_s14 14;
    current_size_test "size-50 current_size" test_board_s50 50;
    current_size_test "size-20 current_size" test_board_s20 20;
    current_size_test "size-30 current_size" test_board_s30 30;
    current_size_test "size-40 current_size" test_board_s40 40;
    current_level_test "size-5 current_level" test_board_s5 0;
    current_level_test "size-2 current_level" test_board_s50 0;
    current_level_test "level 0 for new board of size 2" test_board_s2 0;
    current_level_test "level 0 for new board of size 14" test_board_s14 0;
    current_level_test "level 0 for new board of size 20" test_board_s20 0;
    current_level_test "level 0 for new board of size 30" test_board_s30 0;
    (* testing update level *)
    current_level_test "size-5 level 1" test_board_s5_L1 1;
    current_level_test "size-5 level 2"
      (update_level test_board_s5_L1)
      2;
    current_level_test "size-5  level 9"
      (mult update_level test_board_s2 9)
      9;
    current_level_test "size-5 level 3"
    (mult update_level test_board_s5 3) 3;
    current_level_test "size-5 level 4"
    (mult update_level test_board_s5 4) 4;
    current_level_test "size-5 level 5"
    (mult update_level test_board_s5 5) 5;
    current_level_test "size-5 level 6"
    (mult update_level test_board_s5 6) 6;
    current_level_test "size-5 level 7"
    (mult update_level test_board_s5 7) 7;
    current_level_test "size-5 level 8"
    (mult update_level test_board_s5 8) 8;
    current_level_test "size-5 level 9"
    (mult update_level test_board_s5 9) 9;
    current_level_test "size-14 level 3"
    (mult update_level test_board_s14 3) 3;
    current_level_test "size-20 level 8"
    (mult update_level test_board_s20 8) 8;
    current_level_test "size-30 level 9"
    (mult update_level test_board_s30 9) 9;
    current_level_test "size-40 level 7"
    (mult update_level test_board_s40 7) 7;
    current_size_test "size-2 update round 2"
      (update_round test_board_s2 (1, 1))
      2;
    current_size_test "size-50 update round 2"
      (update_round test_board_s50 (3, 3))
      50;
    current_size_test "size-5 update round 2"
    (update_round test_board_s5 (1, 1)) 5;
    current_score_test "initialized board of size 2" test_board_s2 0;
    current_score_test "initialized board of size 5" test_board_s5 0;
    current_score_test "initialized board of size 14" test_board_s14 0;
    current_score_test "initialized board of size 5'" test_board_s5_L1 0;
    current_score_test "initialized board of size 20" test_board_s20 0;
    current_score_test "initialized board of size 30" test_board_s30 0;
    current_score_test "initialized board of size 40" test_board_s40 0;
    current_score_test "initialized board of size 50" test_board_s50 0;
    current_size_test "size-14 update round"
    (update_round test_board_s14 (1, 1)) 14;
    live_board_test "live median board" live_median_board true;
    live_board_test "dead median board" dead_median_board false;
    live_board_test "live small board" live_small_board true;
    live_board_test "dead small board" dead_small_board false;
    live_board_test "live large board" live_huge_board true;
    live_board_test "dead large board" dead_huge_board false;
    live_board_test "live interface board" live_max_interface true;
    live_board_test "dead interface board" dead_max_interface false;
    live_board_test "live negative_1 board" negative_1 true;
    live_board_test "live negative_n board" negative_n true;
    live_board_test "live left_2 board" left_2 true;
    live_board_test "live left_2_up board" left_2_up false;
    live_board_test "live down_2 board" down_2 true;
    live_board_test "live down_2_up" down_2_up false;
    live_board_test "live no_change_2" no_change_2 false;
    live_board_test "live left_5" left_5 true;
    live_board_test "live left_5_up" left_5_up true;
    live_board_test "live down_5" down_5 true;
    live_board_test "live down_5_up" down_5_up true;
    live_board_test "live no_change_5" no_change_5 true;
    live_board_test "live left_20" left_20 true;
    live_board_test "live left_20_up" left_20_up true;
    live_board_test "live down_20" down_20 true;
    live_board_test "live down_20_up" down_20_up true;
    live_board_test "live all_2" all_2 true;
    live_board_test "live all_2_up" all_2_up false;



  
  ]

let count_negative_test name arr expected_output : test =
  name >:: fun _ -> assert_equal expected_output (count_negative arr)

let update_blocks_test name arr coor (expected_output : int array array)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (update_blocks arr coor)

let string_of_int_tuple tuple =
  "("
  ^ string_of_int (tuple |> fst)
  ^ ", "
  ^ string_of_int (tuple |> snd)
  ^ ")"

let ai_test name arr (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (ai arr) ~printer:string_of_int_tuple

let deep_copy_test name arr (expected_output : int array array) : test = 
  name >:: fun _->
  assert_equal expected_output (deep_copy arr)

let change_tests =
  [
    count_negative_test "negative_0" negative_0 0;
    count_negative_test "count negative on live initialized small board " live_small_board 0;
    count_negative_test "count negative on live initialized median board" live_median_board 0;
    count_negative_test "count negative on kive initialized large board" live_huge_board 0;
    count_negative_test "count negative on dead initialized small board " dead_small_board 0;
    count_negative_test "count negative on dead initialized median board" dead_median_board 0;
    count_negative_test "count negative on dead initialized large board" dead_huge_board 0;
    count_negative_test "count negative on kive initialized max-interface board" live_max_interface 0;
    count_negative_test "count negative on dead initialized max-interface board " dead_max_interface 0;
    count_negative_test "negative_1" negative_1 1;
    count_negative_test "negative_n" negative_n 8;
    count_negative_test "count negative on left_2" left_2 0;
    count_negative_test "count negative on left_2_up" left_2_up 2;
    count_negative_test "count negative on down_2" down_2 0;
    count_negative_test "count negative on down_2_up" down_2_up 2;
    count_negative_test "count negative on left_5" left_5 0;
    count_negative_test "count negative on left_5" left_5_up 5;
    count_negative_test "counet negative on down_5" down_5 0;
    count_negative_test "count negative on down_5_up" down_5_up 10;
    count_negative_test "count negative on no change" no_change_5 0;
    count_negative_test "count negative on left_20" left_20 0;
    count_negative_test "count negative on left_20_up" left_20_up 40;
    count_negative_test "count negative on down_20" down_20 0;
    count_negative_test "count negative on down_20_up" down_20_up 40;
    update_blocks_test "first column empty 2*2" left_2 (0, 0) left_2_up;
    update_blocks_test "bottom row empty 2*2" down_2 (1, 1) down_2_up;
    update_blocks_test "no change 2*2" no_change_2 (0, 0) no_change_2;
    update_blocks_test "third colum empty 5*5" left_5 (0, 2) left_5_up;
    update_blocks_test "blocks fall 5*5" down_5 (1, 0) down_5_up;
    update_blocks_test "no change 5*5" no_change_5 (4, 4) no_change_5;
    update_blocks_test "first two columns empty 20*20" left_20 (0, 0)
      left_20_up;
    update_blocks_test "bottom rows empty 20*20" down_20 (19, 2)
      down_20_up;
    update_blocks_test "no change 20*20" down_20 (19, 0) down_20;
    update_blocks_test "all got eliminated 2*2" all_2 (0, 0) all_2_up;
    ai_test "negative 1 on AI" negative_1 (5, 3);
    ai_test "negative n on AI" negative_n (5, 6);
    ai_test "left_5 up on AI" left_5_up (4, 3);
    ai_test "no change 5 on AI" no_change_5 (0, 3);
    ai_test "live_median_board on AI" live_median_board (4, 4);
    ai_test "dead_median_board on AI" dead_median_board (4, 4);
    ai_test "live_small_board on AI" live_small_board (1, 0);
    ai_test "dead_small_board" dead_small_board (1, 1);
    ai_test "dead_huge_board on AI" dead_huge_board (19, 19);
    ai_test "live_huge_board" live_huge_board (7, 5);
    ai_test "live_max_interfacte" live_max_interface (10, 8);
    ai_test "dead_max_interface on AI" dead_max_interface (13, 13);
    ai_test "negative_0 on AI" negative_0 (1, 1);
    ai_test "down_2 on AI" down_2 (1, 1);
    ai_test "no_change_2 on AI" no_change_2 (1, 1);
    ai_test "left_20_up on AI" left_20_up (7, 3);
    ai_test "down_20_up" down_20_up (9, 5);
    ai_test "all_2 on AI" all_2 (1, 1);
    ai_test "all_2_up on AI" all_2_up (1, 1);
    deep_copy_test "deep copy negative_0" negative_0 negative_0;
    deep_copy_test "deep copy negative_1" negative_1 negative_1;
    deep_copy_test "deep copy negative_n" negative_n negative_n;
    deep_copy_test "deep copy left_2" left_2 left_2;
    deep_copy_test "deep copy down_2" down_2 down_2;
    deep_copy_test "deep copy no_change_2" no_change_2 no_change_2;
    deep_copy_test "deep copy left_5" left_5 left_5;
    deep_copy_test "deep copy down_5" down_5 down_5;
    deep_copy_test "deep copy no_change_5" no_change_5 no_change_5;
    deep_copy_test "deep copy left_5" left_5 left_5;
    deep_copy_test "deep copy down_5" down_5 down_5;
    deep_copy_test "deep copy left_20" left_20 left_20;
    deep_copy_test "deep copy down_20" down_20 down_20;
    deep_copy_test "deep copy live median board" live_median_board 
    live_median_board;
    deep_copy_test "deep copy live small board" live_small_board 
    live_small_board;
    deep_copy_test "deep copy live large board" live_huge_board live_huge_board;
    deep_copy_test "deep copy live max-interface board" live_max_interface 
    live_max_interface;
    deep_copy_test "deep copy dead median board" live_median_board 
    live_median_board;
    deep_copy_test "deep copy dead small board" dead_small_board dead_small_board;
    deep_copy_test "deep copy dead large board" dead_huge_board dead_huge_board;
    deep_copy_test "deep copy dead max-interface board" dead_max_interface 
    dead_max_interface;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [ adventure_tests; command_tests; state_tests; change_tests ]

let _ = run_test_tt_main suite
