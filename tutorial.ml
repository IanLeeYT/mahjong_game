open Tiles

(** [random_abc_gen ()] generate a random set of sequence of characters,
    bamboos, and dots. Randomness is based on the float values of system
    time (around ~ 1/100 second). *)
let random_abc_gen () =
  let i = Unix.gettimeofday () in
  let bound = int_of_float i mod 6 in
  if bound = 0 then [ 100; 200; 300 ]
  else if bound = 1 then [ 200; 300; 100 ]
  else if bound = 2 then [ 300; 100; 200 ]
  else if bound = 3 then [ 100; 300; 200 ]
  else if bound = 4 then [ 300; 200; 100 ]
  else [ 200; 100; 300 ]

(** [random_a ()] generate a random number for set A, a number
    representing the value of the character,and the Randomness is based
    on the float values of system time (around ~ 1/100 second). *)
let random_a () =
  let i = 10. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 5 in
  bound + 1

(** [random_b ()] generate a random number for set B, a number
    representing the value of the bamboo, and the Randomness is based on
    the float values of system time (around ~ 1/100 second). *)
let random_b () =
  let i = 100. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 6 in
  bound + 2

(** [random_c ()] generate a random number for set C, a number
    representing the value of the dots, and the Randomness is based on
    the float values of system time (around ~ 1/100 second). *)
let random_c () =
  let i = 1000. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 7 in
  bound + 3

(** [random_d ()] generate a random number for set D, a number
    representing the value of the winds, and the Randomness is based on
    the float values of system time (around ~ 1/100 second). *)
let random_d () =
  let i = 10000. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 4 in
  bound + 1

(** [random_e ()] generate a random number for set E, a number
    representing the value of the bonus, and the Randomness is based on
    the float values of system time (around ~ 1/100 second). *)
let random_e () =
  let i = 100000. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 2 in
  bound + 5

(** [initial_int_list ()] generate a random hands which is chow valid,
    twice, pung valid, once, and kong valid, once. the Randomness is
    based on the float values of system time (around ~ 1/100 second). *)
let initial_int_list () =
  match random_abc_gen () with
  | [ ah; bh; ch ] ->
      let a = random_a () in
      let b = random_b () in
      let c = random_c () in
      let d = random_d () in
      let e = random_e () in
      [
        [
          ah + a;
          ah + a + 2;
          ah + 9;
          bh + b;
          bh + b + 1;
          ch + 1;
          ch + c;
          ch + c;
          600 + (11 * d);
          600 + (11 * d);
          600 + (11 * e);
          600 + (11 * e);
          600 + (11 * e);
        ];
        [ ah + a + 1; bh + b - 1; bh + b + 2 ];
        [ 600 + (11 * e); ch + c; 600 + (11 * d) ];
        [ ah + 9; ch + 1; 677 ];
      ]
  | _ -> []

(** [sne ()] will make the system sleep and then endline *)
let sne () =
  Unix.sleepf 0.6;
  print_endline " "

(** display toturial from chapter [int] *)
let rec tutorial (chapter : int) : unit =
  if chapter = 0 then chapter_0 ()
  else if chapter = 1 then chapter_1 ()
  else if chapter = 2 then chapter_2 ()
  else if chapter = 3 then chapter_3 ()
  else if chapter = 4 then chapter_4 ()
  else ()

(** [chapter_0 ()] will display tutorial of chapter 0 *)
and chapter_0 () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\n\nWelcome to the Mahjong Game Tutorial.\n";
  print_endline
    "This tutorial will cover the following information. To view a \
     desired chapter, enter the index of the chapter and follow by \
     enter.";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.blue ] "1. Basics";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.cyan ] "2. Tiles";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.green ] "3. Game";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.yellow ] "4. Scoring";
  sne ();
  ANSITerminal.print_string
    [ ANSITerminal.magenta; ANSITerminal.Underlined ]
    "5. Return to Main Menu\n";
  print_endline "Please select from 1 to 5:\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline "Please select from 1 to 5:";
      print_string "> "
  | anystring -> (
      match int_of_string_opt anystring with
      | None -> tutorial 0
      | Some int -> tutorial int)

(** [chapter_1 ()] will display tutorial of chapter 1 *)
and chapter_1 () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "1. Basics\n";
  print_endline
    "Mahjong (Not Solitaire!) is a 4-player tile-based game.";
  Unix.sleep 1;
  print_endline
    "Like the card game rummy, the players sequencely draw and discard \
     tiles, and compete to met the winning goals.";
  print_endline "";
  Unix.sleep 2;
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "However, in our implementation of the game, you will play against \
     three AI players to achieve the same goals.";
  print_endline "";
  Unix.sleep 1;
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.red ]
    "There will be two modes: Easy, with amature AIs; and Hard, with \
     very smart AIs.";
  print_endline "";
  Unix.sleep 2;
  print_endline
    "That's all. Press \"Enter\" to continue. Enter \"Quit\" to return \
     to Main Menu";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline "Press enter to continue.";
      print_string "> "
  | anything -> if anything = "Quit" then () else tutorial 2

(** [chapter_2 ()] will display tutorial of chapter 2 *)
and chapter_2 () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "2. Tiles\n";
  print_endline
    "The game is based on a set of 144 tiles (like cards) based on \
     Chinese characters and symbols.";
  Unix.sleep 2;
  print_endline
    "144 tiles in this game are categorized into 5 groups:\n\
    \      Suits A - Bamboos (36 tiles)\n\
    \      Suits B - Dots (36 tiles)\n\
    \      Suits C - cCharacters (36 tiles)\n\
    \      Honors (28 tiles)\n\
    \      Bonuses (8 tiles)";
  Unix.sleep 2;
  print_endline
    "The three suits are very similar: each of them has numbers 1 to \
     9, and each number has 4 duplicates, so that 4x9=36.\n\
     🀇\t🀈\t🀉\t🀊\t🀋\t🀌\t🀍\t🀎\t🀏\t\n\
     🀐\t🀑\t🀒\t🀓\t🀔\t🀕\t🀖\t🀗\t🀘\t\n\
     🀙\t🀚\t🀛\t🀜\t🀝\t🀞\t🀟\t🀠\t🀡\t";
  Unix.sleep 2;
  print_endline
    "The honors are composed of 7 different types of tiles, namely \
     {🀀\t🀁\t🀂\t🀃\t🀄\t🀅\t🀆\t}, and each tile has 4 \
     duplicates.";
  Unix.sleep 2;
  print_endline
    "The bonuses are composed of 8 different tiles with no duplicates, \
     namely {🀢\t🀣\t🀤\t🀥\t🀦\t🀧\t🀨\t🀩}.";
  Unix.sleep 2;
  print_endline
    "All Tiles has identical backface, so that only you know your \
     tiles. Press enter to continue. Enter \"Quit\" to return to Main \
     Menu.";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline "Press enter to continue.";
      print_string "> "
  | anything -> if anything = "Quit" then () else tutorial 3

(** [chapter_3 ()] will display tutorial of chapter 3 *)
and chapter_3 () =
  ANSITerminal.print_string [ ANSITerminal.green ] "3. Game\n";
  print_endline
    "Beginning of each game, all four players draw 13 tiles, while the \
     player on house draw an extra tile and discard one.";
  Unix.sleep 2;
  print_endline
    "Each player in turn, in counterclockwise direction, draws a tile \
     from the wall; the player proceeds to discard a tile (either the \
     tile just drawn, or a tile in the hand) to maintain a hand of 13.";
  Unix.sleepf 2.5;
  print_endline
    "A winning hand consists of 14 tiles. Players must win by either \
     drawing a piece from the wall that completes a 14-tile hand \
     (winning from the wall) or claiming a discard from another player \
     (winning by discard). The winning hand is either made of four \
     melds and one eyes (standard), or seven eyes (all eyes), or all \
     knitted tiles (explained later.) ";
  Unix.sleep 3;
  print_endline
    "Melds are groups of tiles within the player's hand. It may be \
     chow, pong, or kong. Chow is three suited tiles in sequence. Pong \
     is a set of three identical tiles. Kong is a set of four \
     identical tiles.";
  Unix.sleep 2;
  print_endline
    "There are two ways of forming such melds, either by drawing them \
     sequencely at the wall, or utilize other's discard. Inthe second \
     case, you will have to declear the type of the meld.";
  Unix.sleep 1;
  print_endline
    "As for this reason, each time 1. you draw a tile; 2. other player \
     discard a tile; we will wait for you to input a command before \
     the game could continue. When you draw a card, simply hit discard \
     + index or index to discard the tile at such index.";
  Unix.sleep 2;
  print_endline
    "When another player discard a tile, you may commit no action, and \
     simply hit enter, or continue. Or, you can declear the meld you \
     wish to form using the discard. Here we give you several example \
     of Chow and Pung.";
  Unix.sleep 1;
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "For example, this following hand:";
  chapter_3_contd ()

(** [chapter_3_contd ()] will continue display chapter 3 *)
and chapter_3_contd () =
  (* [random_list] generate a random hand to display *)
  let random_list = initial_int_list () in
  match random_list with
  | [ the_list; chow_list; [ kong; punga; pungb ]; others ] -> (
      the_list |> index_to_tiles |> tiles_to_str |> print_str_list;
      sne ();
      print_endline
        "If the player before you played these tiles you may chow:";
      Unix.sleepf 0.3;
      chow_list |> index_to_tiles |> tiles_to_str |> print_str_list;
      sne ();
      print_endline
        "If the player before you played these tiles you may pung:";
      Unix.sleepf 0.3;
      ANSITerminal.print_string [ ANSITerminal.red ]
        ((punga |> index_tile_converter |> tile_string_converter)
        ^ (punga |> index_tile_converter |> tile_string_converter));
      sne ();
      print_endline
        "If the player before you played these tiles you may kong:";
      Unix.sleepf 0.3;
      ANSITerminal.print_string [ ANSITerminal.red ]
        (kong |> index_tile_converter |> tile_string_converter);
      sne ();
      ANSITerminal.print_string
        [ ANSITerminal.Bold; ANSITerminal.red ]
        "If your hands if full of melds, you may mahjong and win!";
      sne ();
      print_endline
        "You may, at any time, enter help to request for hint to \
         continue game. You may enter quit to quit to main menu. You \
         may restart the current round. You will be given feedback to \
         form you command.";
      Unix.sleep 3;
      print_endline
        "Pro-tip: experienced players do not win every round. They \
         only HU a hand with good scores. Press enter to continue. \
         Enter \"Quit\" to return to Main Menu.";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Press enter to continue.";
          print_string "> "
      | anything -> if anything = "Quit" then () else tutorial 4)
  | _ -> failwith "precondition violation"

(** [chapter_4 ()] will display tutorial of chapter 4 *)
and chapter_4 () =
  ANSITerminal.print_string [ ANSITerminal.yellow ] "4. Scoring\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "You may either mahjong from a discard from other players, or from \
     the wall. When from discard, you win from the player of the \
     discard 100 points. When from wall, you win from all three other \
     players each 100 points (300 points in total). ";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "There are several cases where you may double your score won:";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "1. If you never chow, pung, kong other's tiles.";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "2. If all your melds are eyes and pungs";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.green ]
    "3. If you have all of 1 to 9 of any suit";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "There are several cases where you may add points to the score you \
     won:";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.green ]
    "1. Each kong is 100 points, with the exception of concealed kong \
     which worth 200 points.";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "2. Each bonus is 100 points.";
  sne ();
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "3. Win from a kong draw, or Charater of 5 is 100 points.";
  sne ();
  print_endline
    "That's it. Press enter to end tutorial. Enter \"Replay\" to \
     replay from the beginning.";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline "Press enter to continue.";
      print_string "> "
  | anything -> if anything = "Replay" then tutorial 0 else ()

(** [tutorial_start ()] will display tutorial from chapter 0 *)
let tutorial_start () = tutorial 0
