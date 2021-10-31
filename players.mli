(** Representation of the npc of the mahjong game *)

(** [npc] is a basic npc player of the game*)
type npc =
  | One
  | Two
  | Three

(** [advance] is an advanced npc player of the game *)
type advance =
  | One
  | Two
  | Three

(** [player] is a player of a mahjong game *)
type player =
  | Basic of npc
  | Adv of advance
  | User

(** [t] is a representation of a list of players. There are typically 4
    players*)
type t = player list

(** [player_to_string player] is a string representation of the [player] *)
val player_to_string : player -> string

(** [npc_list_to_string t] is a string representation of the npc players *)
val npc_list_to_string : t -> string

(** [basic_players] is a user and three basic npc players *)
val basic_players : t

(** [adv_players] is a user and three advanced npc players *)
val adv_players : t

(** [execute_round npc] is the action the npc takes during its turn in a
    round *)
val execute_round : player -> unit

(** [find_player player t] return the int representation of the player
    in list t. If [not in] list t, return [4] *)
val find_player : player -> t -> int
