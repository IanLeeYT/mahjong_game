open OUnit2
open Command
open RoundState
open GameState
open Players
open Tiles
open Tutorial

(** This module is for the testing of the Tile-based function. *)

(*****************************************************************)
(* Test Plan *)
(*****************************************************************)

(* The testing for this project is divided into two parts: OUnit-based
   test-case testing and Terminal-based play-test testing *)

(* Main components of this project are divided into four sections.
   ********** Section I : Tile-based algorithms (Tiles module)
   ********** Section II: State-based fuctions (Roundstate, Gamestate,
   Player module) ********** Section III: Command parser (Command
   module) ********** Section IV: Tutorial Display (Tutorial module) The
   function of each modules is documented at the coresponding files. *)

(* Noticeably, we employed test-case testing for Section I of the
   project, because it involved mathmatical computation of the validity.
   and possibility of different combinations of tiles. *)

(* For the tile module, although sometimes complicated, different
   compoenets of the tile-based algorithms are separatable. Generally,
   we, below, separated the testing of algorithm into the validity of
   chow, peng, kong, corner cases, and winning of game. Additional
   information are enumrated below as documentation to the test cases. *)

(* This test approch demonstractes the correctness of the system.
   Tile-based algorithms employed both glassbox testing and blackbox
   testing. In glass-box testing, we tested if the algorithm may
   correctly determine the corner cases according to our algorithm. For
   example in the testing of winning validity of the game, our algorithm
   have corner cases where the pung meld may bewild the identification
   of the eyes of the hand. We also employed black-box testing where we
   generate random chow-valid hands according to mahjongs rules and test
   the correctness of the algorithm *)

(* We employed play-test for state section, because the module
   implemented terminal interface. It is not reasonable to test
   functions that take unit input and output unit and exceptions with
   OUnit testcases. Moreover, the functions in states are mostly
   dependent upon each other. That is, the functions involves recursive
   function calls, which intogether builds a sophisticated system .*)

(* We employed play-test for command section, because a lot of the
   content we can only obtain by playing the game, since a lot of the
   game process and functions are based on the command the user enters
   and observe what is printed out in the terminal.*)

(* We employed play-test for tutorial section, since tutorial is
   composed of displaying messages for the player to read. There are not
   mush to test. *)

(* For these section of the game, they are covered by our active
   play-test, as the playing of the game involves different staetes,
   commands, and displaying the tutorial. The developer of the project
   have, together, tested more than 200 rounds of different game play,
   and identified and fixed numerous bugs. We have also demonstrated
   this game to other friends and students in order to gain feedback on
   the clarity of our instructions and checkout unexpected issues. *)

(*****************************************************************)
(* Test Helper functions *)
(*****************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(********************************************************************
   End helper functions.
 ********************************************************************)

(********************************************************************
   Tile-based testing functions.
 ********************************************************************)

(** [chow_valid_index_test string Tiles.t int int Tiles.tile bool] test
    if the chow_valid function correctly determine the validility of the
    hand to chow *)
let chow_valid_index_test
    (name : string)
    (hand : Tiles.t)
    (t1 : int)
    (t2 : int)
    (t3 : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (chow_index_valid hand t1 t2 t3)

(** [chow_valid_depre_test string Tiles.t int int Tiles.tile bool] test
    if the chow_valid_depre function correctly determine the validility
    of the hand to chow *)
let chow_valid_depre_test
    (name : string)
    (hand : Tiles.t)
    (t1 : int)
    (t2 : int)
    (t3 : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (chow_index_valid hand t1 t2 t3)

(** [pung_valid_test string Tiles.t Tiles.tile bool] test if the
    pung_valid function correctly determine the validility of the hand
    to pung *)
let pung_valid_test
    (name : string)
    (hand : Tiles.t)
    (tile : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (pung_valid hand tile)

(** [kong_valid_index_test string Tiles.t Tiles.tile bool] test if the
    kong_valid function correctly determine the validility of the hand
    to kong *)
let kong_valid_test
    (name : string)
    (hand : Tiles.t)
    (tile : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (kong_valid hand tile)

(** [ankong_valid_index_test string Tiles.t int Tiles.tile bool] test if
    the ankong_valid function correctly determine the validility of the
    hand to ankong *)
let ankong_valid_index_test
    (name : string)
    (hand : Tiles.t)
    (tile : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (ankong_index_valid hand tile)

(** [hu_valid_test string Tiles.t Tiles.tile option bool] test if the
    winning_valid function correctly determine the validility of the
    hand to mahjong *)
let hu_test
    (name : string)
    (hand : Tiles.t)
    (tile_option : tile option)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (winning_valid hand [] tile_option)

(** [discard suggestion_test string Tiles.t Tiles.tile] test if the
    discard_suggestion function correctly determine the best tile to
    discard *)
let discard_suggestion_test
    (name : string)
    (hand : Tiles.t)
    (expected_output : tile) : test =
  name >:: fun _ ->
  assert_equal expected_output (discard_suggestion hand)

(********************************************************************
   Tile combinations.
   Glass-box testing unless otherwise indicated.
 ********************************************************************)

(********************************************************************
   The following combinations test the discard suggestion function 
   unless otherwise indicated. 
   Integer representation refer to Tiles module.
 ********************************************************************)

let discard_tiles_1 =
  [
    101; 101; 101; 206; 207; 305; 305; 305; 633; 633; 633; 677; 677; 677;
  ]
  |> index_to_tiles

let discard_tiles_2 =
  [
    101; 101; 101; 205; 205; 207; 305; 306; 633; 633; 633; 677; 677; 677;
  ]
  |> index_to_tiles

let discard_tiles_3 =
  [
    101; 103; 105; 109; 201; 203; 205; 301; 303; 305; 307; 309; 655; 677;
  ]
  |> index_to_tiles

let discard_tiles_4 =
  [
    101; 103; 105; 109; 201; 203; 205; 301; 303; 305; 307; 309; 677; 677;
  ]
  |> index_to_tiles

let discard_tiles_5 =
  [
    101; 104; 106; 108; 108; 205; 301; 301; 302; 302; 303; 303; 666; 666;
  ]
  |> index_to_tiles

let discard_tiles_6 =
  [
    101; 104; 106; 108; 108; 205; 301; 301; 302; 302; 303; 303; 309; 666;
  ]
  |> index_to_tiles

let discard_tiles_7 =
  [
    101; 106; 108; 109; 205; 301; 301; 611; 622; 633; 644; 655; 655; 666;
  ]
  |> index_to_tiles

let discard_tiles_8 =
  [
    103; 108; 202; 205; 206; 207; 302; 303; 304; 305; 309; 611; 611; 677;
  ]
  |> index_to_tiles

(********************************************************************
   The following combinations test the chow, pung, kong function 
   unless otherwise indicated. 
   Integer representation refer to Tiles module.
 ********************************************************************)

let tiles_list_1 =
  [ 104; 106; 109; 305; 306; 201; 203; 203; 633; 633; 677; 677; 677 ]
  |> index_to_tiles

let tiles_list_2 =
  [ 101; 103; 109; 207; 208; 301; 304; 304; 633; 633; 666; 666; 666 ]
  |> index_to_tiles

(********************************************************************
   The following tiles are valid for chowing, konging, punging
    the above combinations.
 ********************************************************************)

let tile_1_chow_a = index_tile_converter 105

let tile_1_chow_b = index_tile_converter 304

let tile_1_pung_a = index_tile_converter 203

let tile_1_pung_b = index_tile_converter 633

let tile_2_pung_b = tile_1_pung_b

let tile_1_kong = index_tile_converter 677

let tile_2_chow_a = index_tile_converter 102

let tile_2_chow_b = index_tile_converter 209

let tile_2_chow_c = index_tile_converter 206

let tile_2_pung_a = index_tile_converter 304

let tile_false = index_tile_converter 611

let tile_2_kong = index_tile_converter 666

(********************************************************************
   The following combinations test the winning_valid function 
   unless otherwise indicated. 
   Integer representation refer to Tiles module.
 ********************************************************************)

let hu_hand_1 =
  index_to_tiles
    [
      107;
      108;
      108;
      108;
      109;
      201;
      202;
      203;
      304;
      305;
      306;
      307;
      308;
      309;
    ]

let hu_hand_2 =
  index_to_tiles
    [
      101;
      102;
      103;
      104;
      105;
      106;
      107;
      108;
      109;
      101;
      102;
      103;
      666;
      666;
    ]

let hu_hand_3 =
  index_to_tiles
    [
      108;
      108;
      109;
      109;
      109;
      206;
      206;
      206;
      304;
      305;
      306;
      307;
      308;
      309;
    ]

let hu_hand_3_option =
  index_to_tiles
    [ 108; 108; 109; 109; 109; 206; 206; 304; 305; 306; 307; 308; 309 ]

let hu_hand_4 =
  index_to_tiles
    [
      107;
      107;
      107;
      108;
      109;
      206;
      206;
      206;
      304;
      305;
      306;
      307;
      308;
      309;
    ]

let hu_hand_5 =
  index_to_tiles
    [
      106;
      106;
      107;
      108;
      109;
      206;
      206;
      206;
      304;
      305;
      306;
      307;
      308;
      309;
    ]

let hu_hand_6 =
  index_to_tiles
    [
      107;
      108;
      109;
      206;
      206;
      206;
      304;
      305;
      306;
      307;
      308;
      309;
      309;
      309;
    ]

let hu_hand_7 =
  index_to_tiles
    [
      105;
      106;
      107;
      108;
      109;
      206;
      206;
      206;
      304;
      305;
      306;
      307;
      308;
      309;
    ]

let hu_hand_8 =
  index_to_tiles
    [
      107;
      108;
      109;
      206;
      206;
      206;
      304;
      305;
      306;
      307;
      308;
      309;
      309;
      644;
    ]

let hu_hand_9 =
  index_to_tiles [ 105; 105; 105; 108; 108; 207; 208; 209 ]

(********************************************************************
   List of tests.
 ********************************************************************)

(** tiles_tests is the chow, pung, kong test list *)
let tiles_tests =
  [
    (* chow *)
    chow_valid_index_test "c1a" tiles_list_1 1 2 tile_1_chow_a true;
    chow_valid_index_test "c1b" tiles_list_1 4 5 tile_1_chow_b true;
    chow_valid_index_test "c1n" tiles_list_1 1 2 tile_2_chow_b false;
    chow_valid_index_test "c2a" tiles_list_2 1 2 tile_2_chow_a true;
    chow_valid_index_test "c2b" tiles_list_2 4 5 tile_2_chow_b true;
    chow_valid_index_test "c2c" tiles_list_2 4 5 tile_2_chow_c true;
    chow_valid_index_test "c2n1" tiles_list_2 4 5 tile_1_chow_a false;
    chow_valid_index_test "c2n2" tiles_list_2 4 5 tile_1_chow_b false;
    chow_valid_index_test "c2n3" tiles_list_2 4 5 tile_false false;
    (* chow_depre *)
    chow_valid_depre_test "c1a" tiles_list_1 1 2 tile_1_chow_a true;
    chow_valid_depre_test "c1b" tiles_list_1 4 5 tile_1_chow_b true;
    chow_valid_depre_test "c1n" tiles_list_1 1 2 tile_2_chow_b false;
    chow_valid_depre_test "c2a" tiles_list_2 1 2 tile_2_chow_a true;
    chow_valid_depre_test "c2b" tiles_list_2 4 5 tile_2_chow_b true;
    chow_valid_depre_test "c2c" tiles_list_2 4 5 tile_2_chow_c true;
    chow_valid_depre_test "c2n1" tiles_list_2 4 5 tile_1_chow_a false;
    chow_valid_depre_test "c2n2" tiles_list_2 4 5 tile_1_chow_b false;
    chow_valid_depre_test "c2n3" tiles_list_2 4 5 tile_false false;
    (* pung *)
    pung_valid_test "p1a" tiles_list_1 tile_1_pung_a true;
    pung_valid_test "p1b" tiles_list_1 tile_1_pung_b true;
    pung_valid_test "p1n1" tiles_list_1 tile_2_pung_a false;
    pung_valid_test "p1n2" tiles_list_1 tile_false false;
    pung_valid_test "p2a" tiles_list_2 tile_2_pung_a true;
    pung_valid_test "p2b" tiles_list_2 tile_2_pung_b true;
    pung_valid_test "p2n1" tiles_list_2 tile_1_pung_a false;
    pung_valid_test "p2n2" tiles_list_2 tile_false false;
    (* kong *)
    kong_valid_test "k1" tiles_list_1 tile_1_kong true;
    kong_valid_test "k1n1" tiles_list_1 tile_2_kong false;
    kong_valid_test "k1n2" tiles_list_1 tile_false false;
    kong_valid_test "k2" tiles_list_2 tile_2_kong true;
    kong_valid_test "k2n1" tiles_list_2 tile_1_kong false;
    kong_valid_test "k2n2" tiles_list_2 tile_false false;
    (*ankong*)
    ankong_valid_index_test "ak1n1" tiles_list_1 1 false;
    ankong_valid_index_test "ak1n2" tiles_list_1 11 false;
    ankong_valid_index_test "ak2n1" tiles_list_2 2 false;
    ankong_valid_index_test "ak2n2" tiles_list_2 12 false;
  ]

(********************************************************************
   Random Tests.
   Black-box testing unless otherwise indicated.
 ********************************************************************)

(** [random_Test ()] is tests the chow, pung, kong functions *)
let random_tests () =
  (* random generator. The random generator generate tiles based on the
     float digits of the current time (less than ~1/1000 second). Thus,
     we employ multiple random testing, intersected by other testing. *)
  let random_list = initial_int_list () in
  match random_list with
  | [ the_list; chow; pung; others ] -> (
      match chow with
      | [ chowa; chowb; chowc ] -> (
          match pung with
          | [ kong; punga; pungb ] -> (
              match others with
              | [ oa; ob; oc ] ->
                  [
                    chow_valid_index_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowa)
                      true;
                    chow_valid_index_test "chow 4 5 random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowa)
                      false;
                    chow_valid_index_test "chow 4 5 random b"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowb)
                      true;
                    chow_valid_index_test "chow 4 5 random c"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowc)
                      true;
                    chow_valid_index_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowc)
                      false;
                    chow_valid_index_test "chow 6 7 random a"
                      (index_to_tiles the_list)
                      6 7
                      (index_tile_converter chowa)
                      false;
                    chow_valid_index_test "chow 7 8 random a"
                      (index_to_tiles the_list)
                      7 8
                      (index_tile_converter chowb)
                      false;
                    chow_valid_index_test "chow 8 9 random a"
                      (index_to_tiles the_list)
                      8 9
                      (index_tile_converter chowc)
                      false;
                    chow_valid_index_test "chow 2 3 random a"
                      (index_to_tiles the_list)
                      2 3
                      (index_tile_converter chowa)
                      false;
                    chow_valid_index_test "chow 3 4 random a"
                      (index_to_tiles the_list)
                      3 4
                      (index_tile_converter chowb)
                      false;
                    chow_valid_index_test "chow 9 10 random a"
                      (index_to_tiles the_list)
                      9 10
                      (index_tile_converter chowc)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oa)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter ob)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oc)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oa)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter ob)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oc)
                      false;
                    chow_valid_depre_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowa)
                      true;
                    chow_valid_depre_test "chow 4 5 random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowa)
                      false;
                    chow_valid_depre_test "chow 4 5 random b"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowb)
                      true;
                    chow_valid_depre_test "chow 4 5 random c"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowc)
                      true;
                    chow_valid_depre_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowc)
                      false;
                    chow_valid_depre_test "chow 6 7 random a"
                      (index_to_tiles the_list)
                      6 7
                      (index_tile_converter chowa)
                      false;
                    chow_valid_depre_test "chow 7 8 random a"
                      (index_to_tiles the_list)
                      7 8
                      (index_tile_converter chowb)
                      false;
                    chow_valid_depre_test "chow 8 9 random a"
                      (index_to_tiles the_list)
                      8 9
                      (index_tile_converter chowc)
                      false;
                    chow_valid_depre_test "chow 2 3 random a"
                      (index_to_tiles the_list)
                      2 3
                      (index_tile_converter chowa)
                      false;
                    chow_valid_depre_test "chow 3 4 random a"
                      (index_to_tiles the_list)
                      3 4
                      (index_tile_converter chowb)
                      false;
                    chow_valid_depre_test "chow 9 10 random a"
                      (index_to_tiles the_list)
                      9 10
                      (index_tile_converter chowc)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oa)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter ob)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oc)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oa)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter ob)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oc)
                      false;
                    pung_valid_test "pung a random"
                      (index_to_tiles the_list)
                      (index_tile_converter punga)
                      true;
                    pung_valid_test "pung b random"
                      (index_to_tiles the_list)
                      (index_tile_converter pungb)
                      true;
                    pung_valid_test "pung false b random"
                      (index_to_tiles the_list)
                      (index_tile_converter ob)
                      false;
                    pung_valid_test "pung false a random"
                      (index_to_tiles the_list)
                      (index_tile_converter oa)
                      false;
                    kong_valid_test "kong random"
                      (index_to_tiles the_list)
                      (index_tile_converter kong)
                      true;
                    kong_valid_test "kong false a random"
                      (index_to_tiles the_list)
                      (index_tile_converter oa)
                      false;
                    kong_valid_test "kong false b random"
                      (index_to_tiles the_list)
                      (index_tile_converter ob)
                      false;
                    kong_valid_test "kong false c random"
                      (index_to_tiles the_list)
                      (index_tile_converter oc)
                      false;
                  ]
              | _ -> failwith "precondition violation")
          | _ -> failwith "precondition violation")
      | _ -> failwith "precondition violation")
  | _ -> failwith "precondition violation"

(********************************************************************
  List of tests. Glass-box testing unless otherwise indicated.
  ********************************************************************)

(** win_tests is the winning validity of the hands above *)
let win_test =
  [
    hu_test "hu_test_1" hu_hand_1 None true;
    hu_test "hu_test_2" hu_hand_2 None true;
    hu_test "hu_test_3" hu_hand_3 None true;
    hu_test "hu_test_3_option" hu_hand_3_option
      (Some (index_tile_converter 206))
      true;
    hu_test "hu_test_4" hu_hand_4 None true;
    hu_test "hu_test_5" hu_hand_5 None true;
    hu_test "hu_test_6" hu_hand_6 None true;
    hu_test "hu_test_7" hu_hand_7 None false;
    hu_test "hu_test_8" hu_hand_8 None false;
    hu_test "hu_test_9" hu_hand_9 None true;
  ]

(** discard suggestion_tests is the test the discard suggestion of the
    hands above *)
let discard_suggestion_tests =
  [
    discard_suggestion_test "discard test 1" hu_hand_2
      (index_tile_converter 666);
    discard_suggestion_test "discard test 2" discard_tiles_1
      (index_tile_converter 207);
    discard_suggestion_test "discard test 3" discard_tiles_2
      (index_tile_converter 207);
    discard_suggestion_test "discard test 4" discard_tiles_3
      (index_tile_converter 655);
    discard_suggestion_test "discard test 5" discard_tiles_4
      (index_tile_converter 309);
    discard_suggestion_test "discard test 6" discard_tiles_5
      (index_tile_converter 101);
    discard_suggestion_test "discard test 7" discard_tiles_6
      (index_tile_converter 666);
    discard_suggestion_test "discard test 8" discard_tiles_7
      (index_tile_converter 611);
    discard_suggestion_test "discard test 9" discard_tiles_8
      (index_tile_converter 677);
  ]

(** suite represent a list of all active test suites *)
let suite =
  "Mahjong test suite"
  >::: List.flatten
         [
           (* The random generator generate tiles based on the float
              digits of the current time (less than ~1/1000 second).
              Thus, we employ multiple random testing, intersected by
              other testing. *)
           random_tests ();
           discard_suggestion_tests;
           random_tests ();
           tiles_tests;
           random_tests ();
           win_test;
           random_tests ();
         ]

let _ = run_test_tt_main suite
