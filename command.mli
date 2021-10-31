(** Representation of a parser of string from the command line. *)

(** The type [command] is a parsed player command that is composed of a
    verb and, depending on the command, followed by a player command *)
type command =
  (* anytime, valid *)
  | Quit
  | Restart
  | Played
  | Help
  | Next
  (* anytime, check *)
  | Kong
  | Mahjong
  (* player only, check *)
  | Discard of int
  (* npc only, check *)
  | Continue
  | Pung
  | Chow of (int * int)
  (* admin use only *)
  | Admin of int

(** [Invalid] is an exception that is raised when a command is invalid
    and can not be parsed *)
exception Invalid of string

(** [parse str] translates a player's input command into the
    corresponding [command] if valid. If [str] is empty, then the game
    will continue without further action. Otherwise, the function will
    parse the first word as a verb and the rest, if not empty, will be
    the player_command

    Requires: [str] to be composed of only alphanumeric characters and
    spaces.

    exception: Raises: [Invalid] when the player's input command is
    invalid, which corresponds to the following criteria: *)
val parse : string -> command
