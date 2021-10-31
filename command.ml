(** The type [player_command] is a player command phrase where each
    element is word within the phrase. A word is defined to be a
    sequence of non-space characters Do not expose to outside, mli. *)
type player_command = string list

type command =
  | Quit
  | Restart
  | Played
  | Help
  | Next
  | Kong
  | Mahjong
  | Discard of int
  | Continue
  | Pung
  | Chow of (int * int)
  | Admin of int

exception Invalid of string

(* [remove_space str_list] is [str_list] with all spaces removed *)
let remove_space (str_list : player_command) : player_command =
  List.fold_left
    (fun acc str -> if str = "" then acc else str :: acc)
    [] (List.rev str_list)

(* [to_lower lst] is [lst] with all capitalized characters lowercased *)
let to_lower (lst : player_command) : player_command =
  List.map String.lowercase_ascii lst

(* [raise_invalid comment] raises the Invalid exception with [comment]*)
let raise_invalid comment = raise (Invalid ("⚠ Invalid: " ^ comment))

let parse str =
  match String.split_on_char ' ' str |> remove_space |> to_lower with
  | [] -> Continue
  | "continue" :: t | "next" :: t -> (
      match t with
      | [] -> Continue
      | _ ->
          raise_invalid
            "Please try 'continue'. Seek tutorial for details.")
  | "help" :: t -> (
      match t with
      | [] -> Help
      | _ ->
          raise_invalid "Please try 'help'. Seek tutorial for details.")
  | "pung" :: t | "peng" :: t -> (
      match t with
      | [] -> Pung
      | _ ->
          raise_invalid "Please try 'pung'. Seek tutorial for details.")
  | "chow" :: t | "chi" :: t -> (
      match t with
      | [] -> raise_invalid "Chow: need two int"
      | [ hd ] -> raise_invalid "Chow: need two int, not one"
      | [ fst; snd ] -> (
          match int_of_string_opt fst with
          | None -> raise_invalid "Chow: '_->int' '_'"
          | Some index_1 -> (
              match int_of_string_opt snd with
              | None -> raise_invalid "chow '_' '_->int'"
              | Some index_2 -> Chow (index_1, index_2)))
      | _ -> raise_invalid "Chow: only two int, not more")
  | "kong" :: t
  | "gang" :: t
  | "angang" :: t
  | "hiddenkong" :: t
  | "ankong" :: t
  | "hidden" :: "kong" :: t
  | "an" :: "gong" :: t -> (
      match t with
      | [] -> Kong
      | _ ->
          raise_invalid "Please try 'kong'. Seek tutorial for details.")
  | "admin" :: t -> (
      match t with
      | [] -> raise_invalid "Use admin with password."
      | [ "password" ] -> Admin 0
      | [ str ] -> (
          match int_of_string_opt str with
          | Some int -> Admin int
          | None -> raise_invalid "Incorrect password.")
      | _ -> raise_invalid "Incorrect password, no space.")
  | "played" :: t | "view" :: "played" :: t -> (
      match t with
      | [] -> Played
      | _ ->
          raise_invalid
            "Please try 'played'. Seek tutorial for details.")
  | "mahjong" :: t | "hu" :: t -> (
      match t with
      | [] -> Mahjong
      | _ ->
          raise_invalid
            "Please try 'mahjong'. Seek tutorial for details.")
  | "quit" :: t -> (
      match t with
      | [] -> Quit
      | _ ->
          raise_invalid "Please try 'quit'. Seek tutorial for details.")
  | "restart" :: t | "new" :: "round" :: t -> (
      match t with
      | [] -> Restart
      | _ ->
          raise_invalid
            "Please try 'new round'. Seek tutorial for details.")
  | "test" :: "draw" :: t | "test" :: "next" :: t -> (
      match t with
      | [] -> Next
      | _ ->
          raise_invalid "Please try 'next'. Seek tutorial for details.")
  | "discard" :: t | "play" :: t | t -> (
      match t with
      | [] ->
          raise_invalid
            "Please discard a tile, indicate with its index as 1 to 14."
      | [ index_str ] -> (
          if index_str = "ian" then
            raise_invalid "🤢ian is just sad☠"
          else if index_str = "matcha" then
            raise_invalid "🐕matcha🐕 is so cute"
          else if index_str = "joy" then
            raise_invalid
              "🐖🐖🐖🐖🐖🐖🐖🐖🐷🐖🐖🐽🐖🐖🐖🐖🐖🐖🐖🐖🐖"
          else
            match int_of_string_opt index_str with
            | None ->
                raise_invalid
                  ("What do you mean by, '" ^ index_str
                 ^ "'? \n\
                   \ To discard a tile, enter a corresponding index. \n\
                   \ For other command, please begin with Chow, Pung, \
                    Kong, Quit, Help, Restart, Mahjong, and Played.")
            | Some index ->
                if index < 0 then
                  raise_invalid
                    "Discard index has to be positive (1 for the left \
                     most tile)"
                else if index > 14 then
                  raise_invalid
                    "Discard index has to be between 1 and 14, \
                     inclusive (1 for the left most tile)"
                else Discard index)
      | _ ->
          raise_invalid
            "This command is not understood.\n\
             Refer to tutorial for help.\n\
             To phrase a command, please begin with Discard, Continue, \
             Chow, Pung, Kong, Quit, Help, Restart, Mahjong, and \
             Played.")
