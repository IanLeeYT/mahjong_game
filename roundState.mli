(** Representation of a dynamic round within the game at a particular
    instance.

    This module is the state of a round within the game. It keeps track
    of the number and identity of tiles left, the tile hand for each
    player that is hidden from other players, the tile hands that are
    shown, and the player of the current turn *)

(** [t] is the state of a round within the games *)
type t

(** [round_end_message] is a message that is generated when a round
    terminates *)
type round_end_message = {
  winner : Players.player option;
  losers : Players.player option;
  score : int;
  hand : Tiles.t;
}

(** [result] is the result of a round after termination *)
type result =
  | Quit_game
  | Unknown_exception of string
  | Round_end of round_end_message

(** start a round of t list of players while t is the house player.
    return result Quit_game when asked to quit game. return result
    Winning of player and score when someone win the round. return
    result end_of_tiles when the round draws. return when the game ends. *)
val start_rounds : Players.player -> Players.t -> bool -> result

(** [locate_player players index] is the player at the [index] in a list
    of four players [players] *)
val locate_player : Players.t -> int -> Players.player
