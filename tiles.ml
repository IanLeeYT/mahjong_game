(**************************************************************************
  tile def start*)

(** [Blank] represents tile_placeholder when there are no valid
    [current_discard]. [true] represents mahjong_valid (for example
    discard updated afte kong). [false] representes mahjong_invalid (for
    example discard updated after chow and pung) *)
type tile =
  | Blank of bool
  | Dots of int
  | Bamboo of int
  | Characters of int
  | East
  | South
  | West
  | North
  | Red
  | Green
  | White
  | Plum
  | Orchid
  | Chrysanthemum
  | Bam
  | Spring
  | Summer
  | Autumn
  | Winter

type t = tile list

(** representation may not be changed to other than list *)
let empty_hand = []

(** [dots_set] is a list set of all dots tiles *)
let dots_set =
  [
    Dots 1;
    Dots 2;
    Dots 3;
    Dots 4;
    Dots 5;
    Dots 6;
    Dots 7;
    Dots 8;
    Dots 9;
  ]

(** [bamboo_set] is a list set of all bamboo tiles *)
let bamboo_set =
  [
    Bamboo 1;
    Bamboo 2;
    Bamboo 3;
    Bamboo 4;
    Bamboo 5;
    Bamboo 6;
    Bamboo 7;
    Bamboo 8;
    Bamboo 9;
  ]

(** [charactesr_set] is a list set of all character tiles *)
let characters_set =
  [
    Characters 1;
    Characters 2;
    Characters 3;
    Characters 4;
    Characters 5;
    Characters 6;
    Characters 7;
    Characters 8;
    Characters 9;
  ]

(** [orientation_set] is a list set of all orientation tiles *)
let orientations_set = [ East; South; West; North; Red; Green; White ]

(** [not_orientation tile] checks whether [tile] is an orientation tile *)
let not_orientation tile =
  match tile with
  | East | South | West | North | Red | Green | White -> false
  | _ -> true

(** [bonuses] is a list set of all bonus tiles *)
let bonuses =
  [ Plum; Orchid; Chrysanthemum; Bam; Spring; Summer; Autumn; Winter ]

(** [is_bonus tile] is true if [tile] is a bonus. Otherwise, false *)
let is_bonus = function
  | Plum | Orchid | Chrysanthemum | Bam | Spring | Summer | Autumn
  | Winter ->
      true
  | _ -> false

(** [all_tiles_variety] is a set of all tiles, each with a quantity of
    one *)
let all_tiles_variety =
  dots_set @ bamboo_set @ characters_set @ orientations_set

let tile_length tiles = List.length tiles

(** [tile_index_converter tile] is the int index that represents the
    tile [tile]*)
let tile_index_converter = function
  | Bamboo (num : int) -> 100 + num
  | Dots (num : int) -> 200 + num
  | Characters (num : int) -> 300 + num
  | Blank true -> 1
  | Blank false -> 0
  | East -> 611
  | South -> 622
  | West -> 633
  | North -> 644
  | Red -> 655
  | Green -> 666
  | White -> 677
  | Plum -> 811
  | Orchid -> 822
  | Chrysanthemum -> 833
  | Bam -> 844
  | Spring -> 855
  | Summer -> 866
  | Autumn -> 877
  | Winter -> 888

(** [tiles_to_index t] is the int index list that represents the tile
    list [t] *)
let tiles_to_index hand = List.map tile_index_converter hand

let index_tile_converter (i : int) =
  if i = 0 then Blank false
  else if i = 1 then Blank true
  else if i < 110 && i > 100 then Bamboo (i - 100)
  else if i < 210 && i > 200 then Dots (i - 200)
  else if i < 310 && i > 300 then Characters (i - 300)
  else if i = 611 then East
  else if i = 622 then South
  else if i = 633 then West
  else if i = 644 then North
  else if i = 655 then Red
  else if i = 666 then Green
  else if i = 677 then White
  else if i = 811 then Plum
  else if i = 822 then Orchid
  else if i = 833 then Chrysanthemum
  else if i = 844 then Bam
  else if i = 855 then Spring
  else if i = 866 then Summer
  else if i = 877 then Autumn
  else if i = 888 then Winter
  else failwith "precondition violation at converter tiles.ml"

let index_to_tiles hand = List.map index_tile_converter hand

(**************************************************************************
  tile define - end - init hand - start*)

(** time () returns a random int from the system time. limit the int to
    at most 10000, at few 0 by the mod in the end *)
let time () = (Unix.time () |> int_of_float) mod 10000

(**randomize the order if a list of tile*)
let shuffle t =
  let compare x y =
    match x with a, b -> ( match y with c, d -> a - c)
  in
  let random_arr =
    List.map (fun a -> (Random.bits () mod time (), a)) t
  in
  let sorted = List.sort compare random_arr in
  List.map snd sorted

(** A list of all the individual tiles with four of each. *)
let all_tiles : t =
  List.map (fun x -> [ x; x; x; x ]) all_tiles_variety
  |> List.flatten |> ( @ ) bonuses

let init_tiles () : t = shuffle all_tiles

(***********************************************************************
  init hand - end - c,p,kong - start*)

(** [compare t1 t2] is a positive int if t1 is greater than t2, a
    negative int if t1 is less than t2, and is zero if t1 is equal to t2 *)
let compare (t1 : tile) (t2 : tile) =
  tile_index_converter t1 - tile_index_converter t2

(** sort a list of tile by the standard order *)
let sort_hand hand = List.sort compare hand

let chow_valid (hand : t) t1 t2 (t3 : tile) =
  match sort_hand [ t1; t2; t3 ] |> tiles_to_index with
  | [ a; b; c ] -> if a + 1 == b && b + 1 == c then true else false
  | _ -> false

(* raise invalid index exception if the player input incorrect index
   when chow *)

exception Invalid_index

(**chow_index_valid (hand : t) (index1 : int) (index2 : int) t3 take one
   tile t3 to chow and two index of the hand t. Assert index 1 and 2 are
   valid position of the tile, meaning, for a standard hand, 1 to 13. *)
let chow_index_valid (hand : t) (index1 : int) (index2 : int) t3 =
  let hand_length = List.length hand in
  if
    index1 < 1 || index2 < 1 || index1 > hand_length
    || index2 > hand_length
  then raise Invalid_index
  else
    chow_valid hand
      (List.nth hand (index1 - 1))
      (List.nth hand (index2 - 1))
      t3

(**count_tile (hand : t) tile (0 : int) return the number of tile in a
   hand *)
let rec count_tile (hand : t) tile (acc : int) =
  match hand with
  | h :: t ->
      if h = tile then count_tile t tile (acc + 1)
      else count_tile t tile acc
  | [] -> acc

let pung_valid hand tile =
  if count_tile hand tile 0 > 1 then true else false

let kong_valid hand tile =
  if count_tile hand tile 0 > 2 then true else false

let ankong_valid hand tile =
  if count_tile hand tile 0 > 3 then true else false

let ankong_index_valid hand (pos : int) =
  let tile = List.nth hand pos in
  if count_tile hand tile 0 > 3 then true else false

(**************************************************************************
  cpkong - end - winning and scoring - start*)

(** [contains_all hand requirement_hand] return true if hand contains at
    least one of each of the tiles in requirement_hand and return false
    otherwise. Prerequirement: [requirement_hand] must be an instance of
    sort_uniq. *)
let rec contains_all hand requirement_hand : bool =
  match requirement_hand with
  | [] -> true
  | h :: t -> if List.mem h hand then contains_all hand t else false

(** [contains_dragon hand] return [true] if the hand contains [dragon]
    and [false] otherwise *)
let contains_dragon hand : bool =
  contains_all hand bamboo_set
  || contains_all hand dots_set
  || contains_all hand characters_set

(** [bonus_only hand] return [true] if the hand is bonus only and return
    [false] otherwise *)
let rec bonus_only hand : bool =
  match hand with
  | h :: t -> if not (is_bonus h) then false else bonus_only t
  | [] -> true

let rec add_tile_to_hand tile = function
  | tile' :: t as hand ->
      if compare tile tile' <= 0 then tile :: hand
      else tile' :: add_tile_to_hand tile t
  | [] -> [ tile ]

(** [check_size_14 hand] is true if [hand] has length 14 and false
    otherwise*)
let check_size_14 hand = if 14 == List.length hand then true else false

(** [int_list_remove_once tile hand] is [hand] with the first occurance
    that matches [tile] removed *)
let rec int_list_remove_once tile = function
  | h :: t -> if h = tile then t else h :: int_list_remove_once tile t
  | [] ->
      failwith "precondition violation at int list remove at win trio"

(** [find_trio_chow tile hand] is a find_trio helper that finds and
    remove chow trio sets, if possible. Return false otherwise *)
let rec find_trio_chow tile hand =
  if List.mem (tile + 1) hand && List.mem (tile + 2) hand then
    let new_hand =
      hand
      |> int_list_remove_once (tile + 1)
      |> int_list_remove_once (tile + 2)
    in
    find_trio new_hand
  else false

(** [find_trio_pung t1 t2 t3 hand] is a find_trio helper that checks if
    [t1] [t2] [t3] forms a pung trio. If so, carry on execution, else
    return false *)
and find_trio_pung t1 t2 t3 hand =
  if t1 = t2 && t2 = t3 then find_trio hand else false

(** [find_trio hand] checks [hands] in sets of trio. If all trios match
    pung or chow requirement, true is returned. Otherwise, false *)
and find_trio = function
  | t1 :: t2 :: t3 :: tail ->
      if
        find_trio_pung t1 t2 t3 tail
        || find_trio_chow t1 (t2 :: t3 :: tail)
      then true
      else false
  | [] -> true
  | _ -> false

let rec find_bunky = function
  | t1 :: t2 :: t3 :: tail ->
      if t1 = t2 && t2 = t3 then find_bunky tail else false
  | [] -> true
  | _ -> false

(** [find_trump checked_hand hand] locates every possible trump location
    and see if it results in a winning hand. If any result does so,
    return true, otherwise, false *)
let rec find_trump checked_hand = function
  | t1 :: t2 :: tail ->
      if t1 = t2 then
        find_trio (checked_hand @ tail)
        || find_trump (checked_hand @ [ t1 ]) (t2 :: tail)
      else find_trump (checked_hand @ [ t1 ]) (t2 :: tail)
  | [ t1 ] -> false
  | [] -> false

let rec find_bunky_trump checked_hand = function
  | t1 :: t2 :: tail ->
      if t1 = t2 then
        find_bunky (checked_hand @ tail)
        || find_bunky_trump (checked_hand @ [ t1 ]) (t2 :: tail)
      else find_bunky_trump (checked_hand @ [ t1 ]) (t2 :: tail)
  | [ t1 ] -> false
  | [] -> false

(** [winning_hand_standard hand open_hand] checks for the standard
    winning pattern, which is the most common. Requires hand to be
    sorted by Tiles.compare *)
let winning_hand_standard hand open_hand =
  find_trump [] (tiles_to_index hand)

let winning_hand_bunky hand = find_bunky_trump [] (tiles_to_index hand)

(** [check_seven hand] is a helper for [winning_hand_seven] with the
    same objective*)
let rec check_seven = function
  | t1 :: t2 :: tail -> if t1 = t2 then check_seven tail else false
  | [] -> true
  | [ h ] -> false

let rec check_ultra_seven = function
  | t1 :: t2 :: t3 :: t4 :: tail ->
      if t1 = t2 && t2 = t3 && t3 = t4 then check_seven tail
      else if t1 = t2 then check_ultra_seven (t3 :: t4 :: tail)
      else false
  | anything -> false

(** [winning_hand_seven hand] checks for a special winning condition
    (corner case). Return true if the hands constitutes 7 eyes. Requires
    hand to be sorted by Tiles.compare *)
let winning_hand_seven hand =
  if check_size_14 hand then check_seven hand else false

let winning_hand_ultra_seven hand =
  if check_size_14 hand then check_ultra_seven hand else false

(** [check_thirteen hand] is a helper for winning_hand_thirteen with the
    same objective*)
let rec check_thirteen = function
  | t1 :: t2 :: tail ->
      if 2 < t1 - t2 then check_thirteen (t2 :: tail) else false
  | t1 :: tail -> true
  | [] -> false

(** [winning_hand_thirteen hand] checks for a special winning condition
    (corner case). Requires hand to be sorted by Tiles.compare *)
let winning_hand_thirteen (hand : t) =
  if check_size_14 hand then check_thirteen (tiles_to_index hand)
  else false

(** [winning_hand hand open_hand current] is 0 if [hand] doesn't match
    winning conidtion. It is an int greater than 0 otherwise. Requires
    [hand] to be sorted according to Tiles.compare *)
let winning_hand (hand : t) (open_hand : t) (current : tile option) =
  let complete_hand =
    match current with
    | None -> hand
    | Some tile -> add_tile_to_hand tile hand
  in
  if winning_hand_bunky complete_hand then 2
  else if winning_hand_standard complete_hand open_hand then 1
  else if winning_hand_ultra_seven complete_hand then 4
  else if winning_hand_seven complete_hand then 2
  else if winning_hand_thirteen complete_hand then 4
  else 0

(** [winnning_valid habd open_hand current] is true if [hand] matches
    winning condition and false otherwise. Requires [hand] to be sorted
    according to Tiles.compare *)
let winning_valid (hand : t) (open_hand : t) (current : tile option) =
  if winning_hand (sort_hand hand) (sort_hand open_hand) current <> 0
  then true
  else false

let scoring hand open_hand (current : tile option) =
  let (score : int ref) = ref (winning_hand hand open_hand current) in
  print_endline ("Base score: " ^ string_of_int !score);
  let check_from_wall =
    match current with
    | None ->
        score := !score * 2;
        print_endline "!win from wall, score double!";
        ()
    | Some (Blank true) ->
        score := !score * 4;
        print_endline "!win from kong, score quatrable!"
    | Some (Blank false) ->
        score := !score * 8;
        print_endline "!win from start, score scale bonus 8!"
    | Some (Characters 5) ->
        score := !score + 2;
        print_endline "!win Chara 5, bonus score 200!"
    | Some some_tile ->
        if tile_index_converter some_tile > 400 then (
          score := !score + 1;
          print_endline "!win from wind, bonus score 100!")
        else ()
  in
  let check_no_open_hand =
    match open_hand with
    | [] ->
        score := !score * 4;
        print_endline "!win with concealed hand, score quatrable!"
    | some_hand ->
        if bonus_only some_hand then (
          score := !score * 2;
          print_endline "!bonus only, score double!")
        else ()
  in
  let check_dragon =
    if contains_dragon open_hand then (
      score := !score * 4;
      print_endline "!win with open dragon, score quatrable!")
    else if contains_dragon hand then (
      score := !score * 8;
      print_endline "!win from concealed dragon, score scale bonus 8!")
    else ()
  in
  print_endline ("Case score: " ^ string_of_int !score);
  check_from_wall;
  check_no_open_hand;
  check_dragon;
  !score

(***********************************************************************
  winning and scoring - end - tile-printer - start*)

let tile_string_converter = function
  | Dots int -> (
      match int with
      | 1 -> "🀙"
      | 2 -> "🀚"
      | 3 -> "🀛"
      | 4 -> "🀜"
      | 5 -> "🀝"
      | 6 -> "🀞"
      | 7 -> "🀟"
      | 8 -> "🀠"
      | 9 -> "🀡"
      | _ -> failwith "precondition violation at converter tiles.ml")
  | Bamboo int -> (
      match int with
      | 1 -> "🀐"
      | 2 -> "🀑"
      | 3 -> "🀒"
      | 4 -> "🀓"
      | 5 -> "🀔"
      | 6 -> "🀕"
      | 7 -> "🀖"
      | 8 -> "🀗"
      | 9 -> "🀘"
      | _ -> failwith "precondition violation at converter tiles.ml")
  | Characters int -> (
      match int with
      | 1 -> "🀇"
      | 2 -> "🀈"
      | 3 -> "🀉"
      | 4 -> "🀊"
      | 5 -> "🀋"
      | 6 -> "🀌"
      | 7 -> "🀍"
      | 8 -> "🀎"
      | 9 -> "🀏"
      | _ -> failwith "precondition violation at converter tiles.ml")
  | Blank _ -> " 🀫 "
  | East -> "🀀"
  | South -> "🀁"
  | West -> "🀂"
  | North -> "🀃"
  | Red -> "🀄"
  | Green -> "🀅"
  | White -> "🀆"
  | Plum -> "🀢"
  | Orchid -> "🀣"
  | Chrysanthemum -> "🀥"
  | Bam -> "🀤"
  | Spring -> "🀦"
  | Summer -> "🀧"
  | Autumn -> "🀨"
  | Winter -> "🀩"

let tiles_to_str hand = List.map tile_string_converter hand

(***********************************************************************
  tile-printer - end *)

(** selfkong_valid open_hand hand is true if one can use one of the hand
    to kong some three in the open hand. AF: the hand is a valid hand.
    if hand does contain such combo then return true *)
let rec selfkong_valid open_hand hand =
  match hand with
  | h :: t ->
      if kong_valid open_hand h then true
      else selfkong_valid open_hand t
  | [] -> false

let rec selfkong_tile open_hand hand =
  match hand with
  | h :: t ->
      if kong_valid open_hand h then h else selfkong_tile open_hand t
  | [] -> failwith "precondition violation at selfkong at tiles.ml"

let ankong_tile_opt hand = List.find_opt (ankong_valid hand) hand

(** ankong_valid_new hand is true if the hand has a valid hidden kong.
    automatically kong the first hidden kong find. AF: the hand is a
    valid hand. if hand does contain such pattern then return true *)
let ankong_valid_new hand =
  match ankong_tile_opt hand with None -> false | Some t -> true

(*********************************************)
(* New add Functions - Not Tested *)
(*********************************************)

(** sort a list of tile by the reverse of standard order *)
let rev_sort_hand hand = List.rev (sort_hand hand)

(** [separate_first_tile hand] returns a pair containing the first tile
    in [hand] and everything else in [hand]. Raises an exception if
    [hand] is empty *)
let separate_first_tile hand =
  match hand with
  | h :: t -> (t, h)
  | [] -> failwith "precondition violation"

(** [separate_random_tile hand] Raises an exception if [hand] is empty *)
let separate_random_tile hand =
  match shuffle hand with
  | h :: t -> (sort_hand t, h)
  | [] -> failwith "precondition violation"

(** [separate_last_tile hand] Raises an exception if [hand] is empty *)
let separate_last_tile hand =
  let rev = rev_sort_hand hand in
  match rev with
  | h :: t -> (t, h)
  | [] -> failwith "precondition violation"

let rec print_str_list = function
  | fst :: snd :: t ->
      print_string (fst ^ " ");
      print_str_list (snd :: t)
  | h :: t -> print_string h
  | [] -> ()

(** requires [hand] contain [tile] for at least quantities of [amount] *)
let rec remove hand tile (amount : int) : t =
  if amount < 1 then hand
  else
    match hand with
    | [] -> failwith "precondition at remove at tiles.ml"
    | h :: t ->
        if h = tile then remove t tile (amount - 1)
        else h :: remove t tile amount

let rec remove_index hand index : t =
  match hand with
  | h :: t ->
      if index = 1 then t
      else if index > 1 then h :: remove_index t (index - 1)
      else failwith "precondition violation at remove at tiles.ml"
  | [] -> failwith "precondition violation at remove at tiles.ml"

let rec chow_remove hand index_1 index_2 : t =
  if index_1 > index_2 then chow_remove hand index_2 index_1
  else remove_index (remove_index hand index_2) index_1

let hu_possible hand =
  match winning_valid hand [] None with
  | exception exn -> false
  | t -> t

let kong_possible hand =
  match ankong_valid_new hand with exception exn -> false | t -> t

let pung_possible = pung_valid

(** [uniq_lst lst tile] is [lst] with one occurence of [tile] *)
let uniq_lst lst tile = if List.mem tile lst then lst else tile :: lst

(** [chow_possible hand tile] detemines whether [hand] can chow [tile].
    [hand] must be sorted according to Tiles.compare *)
let chow_possible hand tile =
  let rec helper hand tile' =
    match hand with
    | [] -> None
    | [ h ] -> None
    | fst :: snd :: t ->
        if chow_valid hand fst snd tile' then Some (fst, snd)
        else helper (snd :: t) tile'
  in
  helper (List.fold_left uniq_lst [] hand |> List.rev) tile

(*********************************************)
(* NPC optimization moves and tile suggestion *)

(** [priorities_orientation not_empty hand] is [hand] with tiles that
    are orientations removed. At least one tile is left in [hand]*)
let rec priorities_orientations not_empty = function
  | h1 :: h2 :: t ->
      if not_orientation h1 then
        priorities_orientations (false || not_empty) (h2 :: t)
      else h1 :: priorities_orientations true (h2 :: t)
  | [ h1 ] as lst -> if not_orientation h1 && not_empty then [] else lst
  | [] -> failwith "Precondition Violation"

(** [is_middle tile] checks whether [tile] is a middle tile, meaning it
    is a tile type with int from 2 to 9, inclusive *)
let is_middle tile =
  match tile with
  | Dots n | Characters n | Bamboo n ->
      if n = 1 || n = 9 then false else true
  | _ -> false

(** [priorities_middles not_empty hand] is [hand] with tiles that are
    middles (2 to 9, inclusive) removed. At least one tile is left in
    [hand]*)
let rec priorities_middles not_empty = function
  | h1 :: h2 :: t ->
      if is_middle h1 then
        priorities_middles (false || not_empty) (h2 :: t)
      else h1 :: priorities_middles true (h2 :: t)
  | [ h1 ] as lst -> if is_middle h1 && not_empty then [] else lst
  | [] -> failwith "Precondition Violation"

(** [priorities_skip_seq not_empty hand] is [hand] with pair tiles that
    are in sequence removed. At least one tile is left in [hand] *)
let rec priorities_skip_seq not_empty = function
  | h1 :: h2 :: h3 :: t ->
      if tile_index_converter h1 + 2 = tile_index_converter h2 then
        priorities_skip_seq (false || not_empty) (h3 :: t)
      else h1 :: priorities_skip_seq true (h2 :: h3 :: t)
  | [ h1 ] as lst -> lst
  | h1 :: h2 :: t as lst ->
      if
        tile_index_converter h1 + 2 = tile_index_converter h2
        && not_empty
      then []
      else lst
  | [] -> failwith "Precondition Violation"

(** [priorities_skip_seq not_empty hand] is [hand] with pair tiles that
    are in sequence removed. At least one tile is left in [hand] *)
let rec priorities_seq not_empty = function
  | h1 :: h2 :: h3 :: t ->
      if tile_index_converter h1 + 1 = tile_index_converter h2 then
        priorities_seq (false || not_empty) (h3 :: t)
      else h1 :: priorities_seq true (h2 :: h3 :: t)
  | [ h1 ] as lst -> lst
  | h1 :: h2 :: t as lst ->
      if
        tile_index_converter h1 + 1 = tile_index_converter h2
        && not_empty
      then []
      else lst
  | [] -> failwith "Precondition Violation"

(** [priorities_pair not_empty hand] is [hand] with tiles that are pairs
    removed. At least one tile is left in [hand] *)
let rec priorities_pairs not_empty = function
  | h1 :: h2 :: h3 :: t ->
      if h1 = h2 then priorities_pairs (false || not_empty) (h3 :: t)
      else h1 :: priorities_pairs true (h2 :: h3 :: t)
  | [ h1 ] as lst -> lst
  | h1 :: h2 :: t as lst -> if h1 = h2 && not_empty then [] else lst
  | [] -> failwith "Precondition Violation"

(** [priorities_chow_pung hand] is [hand] with tiles that meet chow and
    pung requirements removed *)
let rec priorities_chow_pung = function
  | h :: t as hand -> (
      let hand_t, tile_h = separate_first_tile hand in
      if pung_possible hand_t tile_h then
        priorities_chow_pung (remove hand_t tile_h 2)
      else
        match chow_possible hand_t tile_h with
        | Some (t1, t2) ->
            let hand_t = remove hand_t t1 1 in
            let hand_t = remove hand_t t2 1 in
            priorities_chow_pung hand_t
        | None -> tile_h :: priorities_chow_pung hand_t)
  | [] -> []

let give_pre int_orig_hand (current_pref, int_tile) : int * int =
  let update_minus_2 =
    if List.mem (int_tile - 2) int_orig_hand then current_pref + 10
    else current_pref
  in
  let update_minus_1 =
    if List.mem (int_tile - 1) int_orig_hand then update_minus_2 + 30
    else update_minus_2
  in
  let update_plus_1 =
    if List.mem (int_tile + 1) int_orig_hand then update_minus_1 + 30
    else update_minus_1
  in
  let update_plus_2 =
    if List.mem (int_tile + 2) int_orig_hand then update_plus_1 + 10
    else update_plus_1
  in
  let update_self =
    if
      List.length (List.filter (fun x -> x = int_tile) int_orig_hand)
      > 1
    then update_plus_2 + 50
    else update_plus_2
  in
  (update_self, int_tile)

let give_self tile =
  let int_tile = tile_index_converter tile in
  if int_tile > 400 then (0, int_tile)
  else if int_tile mod 10 > 5 then (10 - (int_tile mod 10), int_tile)
  else (int_tile mod 10, int_tile)

let sort_by_preferences orig_hand sortee_hand : t =
  let int_orig_hand = tiles_to_index orig_hand in
  let assoc_hand : (int * int) list = List.map give_self sortee_hand in
  List.sort
    (fun (a, b) (c, d) -> a - c)
    (List.map (give_pre int_orig_hand) assoc_hand)
  |> List.map snd |> index_to_tiles

(** [discard_suggestion hand] suggest a tile t from list of tile, hand,
    to be the best to discard. Requires the hand to not meet mahjong
    standards *)
let discard_suggestion (hand : t) : tile =
  match
    hand |> priorities_chow_pung |> priorities_pairs false
    |> priorities_seq false
    |> priorities_skip_seq false
    |> priorities_middles false
    |> priorities_orientations false
  with
  | [ h ] -> h
  | h :: t as lst -> (
      match sort_by_preferences hand lst with
      | h :: t -> h
      | [] -> failwith "precondition violation")
  | [] -> failwith "precondition violation"

let separate_best_tile (hand : t) : t * tile =
  let best_tile = discard_suggestion hand in
  (remove hand best_tile 1, best_tile)

let count_bonus open_hand : int =
  let rec count_helper open_hand acc : int =
    match open_hand with
    | h :: t ->
        if is_bonus h then count_helper t (acc + 1)
        else count_helper t acc
    | [] -> acc
  in
  count_helper open_hand 0

(** [locate_tile_in_hand_hp hand tile acc] is tail recursive locate tile
    in hand, return position. *)
let rec locate_tile_in_hand_hp hand tile acc : int =
  match hand with
  | [] -> -1
  | h :: t ->
      if h = tile then acc else locate_tile_in_hand_hp t tile (acc + 1)

let locate_tile hand tile : int = locate_tile_in_hand_hp hand tile 1

exception No_such_tile

(** [get_tile_helper acc_hand remain_hand tile_need] is a tail recursive
    function to raise the tile of choice to the first tile of the hand *)
let rec get_tile_helper acc_hand remain_hand tile_need =
  match remain_hand with
  | h :: t ->
      if h = tile_need then (h :: acc_hand) @ t
      else get_tile_helper (acc_hand @ [ h ]) t tile_need
  | [] -> raise No_such_tile

let get_tile_to_top hand tile_int =
  let target_tile =
    match index_tile_converter tile_int with
    | exception Failure f -> raise No_such_tile
    | exception _ -> raise No_such_tile
    | anything -> anything
  in
  get_tile_helper empty_hand hand target_tile
