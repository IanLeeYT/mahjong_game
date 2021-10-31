open Players
open RoundState

type t = {
  round_num : int;
  termination_distance : int;
  scores : int array;
  players : player list;
  house : player;
  house_streak : int;
  house_index : int;
  is_adv : bool;
}

type game_progress =
  | Quit of t
  | Continue of t

(** [index_of_player player_list player] is the index of the [player] in
    a list of four players [players]*)
let index_of_player player_list player =
  let rec helper players_list acc =
    match players_list with
    | [] -> failwith "precondition violation"
    | h :: t -> if h = player then acc else helper t (acc + 1)
  in
  helper player_list 0

(** [random_house_index] is a random interger from 0 to 3 *)
let random_house_index = (Unix.time () |> int_of_float) mod 4

(** [players is_advanced] is the advanced npc if [is_advanced] is true,
    else basic npc *)
let players is_advanced =
  if is_advanced then adv_players else basic_players

let init_game (distance : int) (is_advanced : bool) : t =
  let players = players is_advanced in
  let house_index = random_house_index in
  let house = locate_player players house_index in
  {
    round_num = 1;
    termination_distance = distance;
    scores = [| 5000; 5000; 5000; 5000 |];
    players;
    house;
    house_index;
    house_streak = 0;
    is_adv = is_advanced;
  }

(****************************************************)
(* Functions for deciding whether the game continues or end after a
   round terminated. Updating game state accordingly if game hasn't
   ended *)
(****************************************************)

(** [calculate_score t house winner] is the score that will be added to
    the winner and subtracted from the loser(s) of the last round *)
let calculate_score t house winner =
  let base_score = 2 in
  let house_bonus = if house = winner then t.house_streak + 1 else 0 in
  base_score + house_bonus

(** [update_score t house winner losers tile_score] is the game state
    [t] with updated player scores *)
let update_score t house winner losers tile_score =
  let winner_index = index_of_player t.players winner in
  let score = (calculate_score t house winner + tile_score) * 100 in
  match losers with
  | None ->
      t.scores.(winner_index) <- t.scores.(winner_index) + (score * 3);
      Array.iteri
        (fun i s ->
          if i = winner_index then ()
          else t.scores.(i) <- t.scores.(i) - score)
        t.scores
  | Some loser ->
      t.scores.(winner_index) <- t.scores.(winner_index) + score;
      let loser_index = index_of_player t.players loser in
      t.scores.(loser_index) <- t.scores.(loser_index) - score

(** [reached_termination t house_wins] is whether the current game state
    is at termination or not *)
let reached_termination t house_wins =
  t.termination_distance <= 1 && not house_wins

(** [continue_or_quit t house house_wins] returns the updated wrapped
    game state in Quit or Continue *)
let continue_or_quit t house house_wins =
  if reached_termination t house_wins then Quit t
  else
    let new_round_number = t.round_num + 1 in
    let new_termination_distance =
      if house_wins then t.termination_distance
      else t.termination_distance - 1
    in
    let new_house_index =
      if house_wins then t.house_index else (t.house_index + 1) mod 4
    in
    let new_house_wins = if house_wins then t.house_streak + 1 else 0 in
    let new_house = locate_player t.players new_house_index in
    Continue
      {
        t with
        round_num = new_round_number;
        termination_distance = new_termination_distance;
        house_index = new_house_index;
        house = new_house;
        house_streak = new_house_wins;
      }

(** [update_game_state t winning_message] is the updated game state
    after a round has ended. It is wrapped by [game_progress] to
    indicate the updated state should quit or continue next *)
let update_game_state t winning_message =
  let house = locate_player t.players t.house_index in
  match winning_message.winner with
  | None ->
      let house_wins = false in
      continue_or_quit t house house_wins
  | Some winner ->
      let tile_score = winning_message.score in
      let house_wins = house = winner in
      update_score t house winner winning_message.losers tile_score;
      continue_or_quit t house house_wins

let rec update t =
  match start_rounds t.house t.players t.is_adv with
  | Quit_game -> Quit t
  | Round_end winning_message -> update_game_state t winning_message
  | Unknown_exception str ->
      print_endline ("Unknown Exception Caught" ^ str);
      Quit t

(** [get_score t] is the scores of the four player in the current game
    state [t]*)
let get_score t = t.scores

let get_round t = t.round_num

let string_of_scores t =
  let score = get_score t in
  player_to_string (locate_player t.players 0)
  ^ ": "
  ^ string_of_int score.(0)
  ^ "\n"
  ^ player_to_string (locate_player t.players 1)
  ^ ": "
  ^ string_of_int score.(1)
  ^ "\n"
  ^ player_to_string (locate_player t.players 2)
  ^ ": "
  ^ string_of_int score.(2)
  ^ "\n"
  ^ player_to_string (locate_player t.players 3)
  ^ ": "
  ^ string_of_int score.(3)
