(** Representation of an instance of tutorial display

    This module is a representation of the tutorial, which is composed
    of 5 chapters and the display last for few minutes. Tutorial may be
    called from module main to start the display. *)

(** [tutorial_start int] will display toturial from chapter [int] *)
val tutorial : int -> unit

(** [tutorial_start ()] will display toturial from chapter 0 *)
val tutorial_start : unit -> unit

(************************************************************************
  The following functions are for testing purposes only. Do not use
  anywhere other than in test.ml.
  ***********************************************************************)

(** [Testing Only] [initial_int_list ()] generate a random hands which
    is chow valid, twice, pung valid, once, and kong valid, once. the
    Randomness is based on the float values of system time (around ~
    1/100 second). *)
val initial_int_list : unit -> int list list
