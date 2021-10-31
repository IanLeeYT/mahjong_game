(** Representation of tiles *)

(****************************************************)
(* definition of hand and tiles *)
(****************************************************)

(** Representation of one tile*)
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

(** Representation of the tiles left in a mahjong game round. Initialize
    with 144 tiles*)
type t = tile list

(** representation of the empty hand *)
val empty_hand : t

(** Initialize with 144 tiles*)
val all_tiles : t

(** [init_tiles ()] is a randomly shuffled set that contain all 144
    tiles*)
val init_tiles : unit -> t

(** [tile_length hand] is the length of [hand] *)
val tile_length : t -> int

(** true if tile is a bonus *)
val is_bonus : tile -> bool

(****************************************************)
(* validity of action and winning *)
(****************************************************)

(** [Invalid_index] is an exception representing invalid index exception
    if the player input incorrect index when chow. Namely, the game
    raise invalid index exception if the player input incorrect index
    when chow *)
exception Invalid_index

(** [chow_index_valid hand t1 t2 t3] is true if t1 t2 t3 is a chow. \n
    AF: the (hand : tile list) must already contain t1 and t2, otherwise
    it is not a valid chow. In other words, if a person chow 1 2 to 3
    without having 1 2, system will retrun 'invalid command, no such 1
    and 2'. else, here to determine if 1 2 3 forms a valid chow, and if
    not return 'invalid chow, 1 2 3 is not a chow' exception raise
    Invalid_index if the index is not valid *)
val chow_index_valid : t -> int -> int -> tile -> bool

(** [pung_valid hand tile] is true when [tile] can form a pung
    combination with [hand] and false otherwise*)
val pung_valid : t -> tile -> bool

(** [kong_valid hand tile] is true when [tile] can form a kong
    combination with [hand] and false otherwise*)
val kong_valid : t -> tile -> bool

(** [kong_valid hand tile] is true when [tile] can form a kong
    combination with [hand] using the tile at index pos and false
    otherwise*)
val ankong_index_valid : t -> int -> bool

(** [winning_hand hand open_hand] is true when [hand] is a valid winning
    hand in mahjong and false otherwise*)
val winning_valid : t -> t -> tile option -> bool

(** selfkong_valid open_hand hand is true if one can use one of the hand
    to kong some three in the open hand. AF: the hand is a valid hand.
    if hand does contain such combo then return true *)
val selfkong_valid : t -> t -> bool

(** [selfkong_tile hand1 hand2] is the tile in [hand2] that can be self
    kong onto [hand1] *)
val selfkong_tile : t -> t -> tile

(** ankong_valid_new hand is true if the hand has a valid hidden kong.
    automatically kong the first hidden kong find. AF: the hand is a
    valid hand. if hand does contain such pattern then return true *)
val ankong_valid_new : t -> bool

(** [ankong_tile_opt hand] is an option containing the tile that matches
    the ankong requirement, if any *)
val ankong_tile_opt : t -> tile option

(****************************************************)
(* scoring *)
(****************************************************)

(** [count_bonus open_hand] return the number of bonus in the open_hand *)
val count_bonus : t -> int

(** [scoring hand open_hand] is the amount of points that is awarded to
    the player with the winning [hand]*)
val scoring : t -> t -> tile option -> int

(****************************************************)
(* converter: tiles, string, and int *)
(****************************************************)

(** [index_to_tiles t] is the tile list that represents the tile index
    list [t] *)
val index_to_tiles : int list -> t

(** [index_tile_converter n] is the tile that represents the tile index
    [n] *)
val index_tile_converter : int -> tile

(** [tiles_to_str t] is the string list that represents the tile list
    [t] *)
val tiles_to_str : t -> string list

(** [tile_string_converter tile] is the unicode string that represents
    [tile]*)
val tile_string_converter : tile -> string

(** [print_str_list lst] prints the string list [lst] and returns unit *)
val print_str_list : string list -> unit

(** [locate_tile hand tile acc] returns the location (first occorance)
    of the tile in the hand *)
val locate_tile : t -> tile -> int

(****************************************************)
(* sort, spearate, and remove *)
(****************************************************)

(** [remove hand tile amount] is the tile list [t] with [amount] [tile]
    removed *)
val remove : t -> tile -> int -> t

(** [remove_index hand n] is the tile list [hand] with the [n]th tile
    removed*)
val remove_index : t -> int -> t

(** [chow_remove hand n1 n2] is the tile list [hand] where the indeces
    [n1] and [n2] are removed. *)
val chow_remove : t -> int -> int -> t

(** separate the last tile from the list of tiles *)
val separate_last_tile : t -> t * tile

(** separate a random tile from the list of tiles *)
val separate_random_tile : t -> t * tile

(** [add_tile_to_hand tile hand] is hand with tiles added *)
val add_tile_to_hand : tile -> t -> t

(** separate a best tile from the list of tiles for adv npc to discard *)
val separate_best_tile : t -> t * tile

(****************************************************)
(* help and suggestion *)
(****************************************************)

(** suggest a tile t from list of tile, hand, to be the best to discard *)
val discard_suggestion : t -> tile

(** determine if a hand is possible to hu a given tile*)
val hu_possible : t -> bool

(** determine if a hand is possible to kong a given tile*)
val kong_possible : t -> bool

(** determine if a hand is possible to chow a given tile*)
val chow_possible : t -> tile -> (tile * tile) option

(** determine if a hand is possible to pung a given tile*)
val pung_possible : t -> tile -> bool

(** [priorities_chow_pung hand] is [hand] with tiles that meet chow and
    pung requirements removed *)
val priorities_chow_pung : t -> t

(****************************************************)
(* admin *)
(****************************************************)

(** such exception is raise when admin raise action failed *)
exception No_such_tile

(** [get_tile_to_top hand tile_int] raise, if such tile exist, such tile
    to the top of the list. If not, raise exception No_such_tile *)
val get_tile_to_top : t -> int -> t
