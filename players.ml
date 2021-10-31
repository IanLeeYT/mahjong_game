type npc =
  | One
  | Two
  | Three

type advance =
  | One
  | Two
  | Three

type player =
  | Basic of npc
  | Adv of advance
  | User

type t = player list

let player_to_string = function
  | Basic basic -> (
      match basic with One -> "Ian" | Two -> "Andrew" | Three -> "Leo")
  | Adv adv -> (
      match adv with
      | One -> "Master Ian"
      | Two -> "Professor Andrew"
      | Three -> "Wizard Leo 🐒")
  | User -> "You"

let npc_list_to_string lst =
  let npcs = List.tl lst in
  let rec helper acc = function
    | [ player ] -> acc ^ player_to_string player
    | player :: t -> helper (acc ^ player_to_string player ^ ", ") t
    | _ -> failwith "precondition violation"
  in
  helper "" npcs

let basic_players : t = [ User; Basic One; Basic Two; Basic Three ]

let adv_players : t = [ User; Adv One; Adv Two; Adv Three ]

let execute_round player = ()

let rec find_player player pl : int =
  match pl with
  | [] -> 0
  | h :: t -> if player = h then 0 else 1 + find_player player t
