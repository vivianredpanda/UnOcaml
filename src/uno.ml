open Hand
open Card
open Deck

(* game file *)

module type Game = sig
  type t

  val get_deck : t -> Deck.t
  val get_curr_card : t -> Card.card
  val get_curr_player : t -> int
  val get_hand : t -> int -> Hand.t
  val get_human_index : t -> int
  val hands_to_list : t -> Card.card list list
  val check_play : t -> Card.card -> bool
  val build : int -> t
  val play_card : t -> Card.card -> Hand.t -> t
  val handle_play : t -> bool -> string -> t
  val robot_turn : t -> int -> t
end

module Game = struct
  (* UNIMPLEMENTED: stacking +2 or +4 *)
  type t = {
    curr_deck : Deck.t;
    curr_card : Card.card;
    curr_player : int;
    hands : Hand.t list;
    human_index : int;
  }

  let get_deck (game : t) : Deck.t = game.curr_deck
  let get_curr_card (game : t) : Card.card = game.curr_card
  let get_curr_player (game : t) : int = game.curr_player
  let get_human_index (game : t) : int = game.human_index

  (* todo: make this work for any n because game can have any number of players
     and doesnt have to be 4 player *)
  let get_hand (game : t) (player_num : int) : Hand.t =
    let hands = game.hands in
    match player_num with
    | 0 -> List.nth hands 0
    | 1 -> List.nth hands 1
    | 2 -> List.nth hands 2
    | 3 -> List.nth hands 3
    | _ -> failwith "invalid player number"

  let hands_to_list (game : t) : Card.card list list =
    let rec to_list_list hands =
      match hands with
      | [] -> []
      | hand :: t -> Hand.to_list hand :: to_list_list t
    in
    to_list_list game.hands

  let check_play (game : t) (card : Card.card) : bool =
    match card with
    | Wildcard _ | Wildcard4 _ -> true
    | Skip color | Reverse color -> Card.get_color game.curr_card = color
    | Number (num, color) ->
        if Card.get_color game.curr_card <> color then
          match Card.get_number game.curr_card with
          | Some n -> n = num
          | None -> false
        else true
    | Plus (num, color) ->
        if Card.get_color game.curr_card <> color then
          match game.curr_card with
          | Plus (num, _) -> begin
              match Card.get_number game.curr_card with
              | Some n -> n = num
              | None -> false
            end
          | _ -> false
        else true

  (* Build helper function. Takes in a deck [deck] to deal from, a list of hands
     [lst], and a number of hands to deal [n]. Deals [n] hands and adds each one
     to the input list. Returns the list of hands and the updated deck. *)
  let rec deal_hands deck lst n =
    match n with
    | 0 -> (lst, deck)
    | n ->
        let new_deck, hand = Deck.deal deck in
        deal_hands new_deck (Hand.of_list hand :: lst) (n - 1)

  let build n =
    let start_deck = Deck.reset () in
    let starting_hands, dealt_deck = deal_hands start_deck [] n in
    let fst_card, deck = Deck.draw dealt_deck in
    {
      curr_deck = deck;
      curr_card = fst_card;
      curr_player = 0;
      hands = starting_hands;
      human_index = 0;
    }

  (* Checks if the given card is a Reverse card *)
  let check_reverse (card : Card.card) =
    match card with
    | Reverse _ -> true
    | _ -> false

  (* Given a list [hands], an index of that list [player], and an element
     [new_hand] to add at that index, replaces the previous element at the index
     with [new_hand] and returns the updated list. *)
  let rec replace (hands : 'b list) (player : int) (new_hand : 'b) =
    match hands with
    | [] -> []
    | h :: t ->
        if player = 0 then new_hand :: t
        else h :: replace t (player - 1) new_hand

  (* Given a card, a player index, and a total number of players, returns the
     index of the next player. Requires 0 <= [player] < [n] - 1. *)
  let next_player (card : Card.card) (player : int) (n : int) =
    match card with
    | Number _ | Plus _ | Wildcard _ | Wildcard4 _ | Reverse _ ->
        if player = n - 1 then 0 else player + 1
    | Skip _ ->
        if player = n - 1 then 1 else if player = n - 2 then 0 else player + 2

  (* To-do: Add non-number card functionality to determine the next_player when
     reverse or skip is played. *)

  (* Given a game state, the index of the current player, the current player's
     hand after playing a card, and the card played, return the list of hands of
     all the players based on the card played, and the updated deck. *)
  let update_hands (game : t) (player : int) (new_hand : 'b) (card : Card.card)
      =
    match card with
    | Number _ | Wildcard _ | Skip _ | Reverse _ ->
        (replace game.hands player new_hand, game.curr_deck)
    | _ -> failwith "Non-number card functionality unimplemented"
  (* To-do: Add non-number card functionality to switch hands around when
     reverse is played, and add cards to the next player's deck if Plus/
     Wildcard4 is played. *)

  let play_card (game : t) (card : Card.card) (new_hand : Hand.t) =
    let player = game.curr_player in
    let hands, new_deck = update_hands game player new_hand card in
    let curr_player_index, hands, human_index =
      if check_reverse card then
        ( List.length game.hands - player - 1,
          List.rev hands,
          List.length hands - 1 )
      else (player, hands, game.human_index)
    in
    let next_player = next_player card curr_player_index (List.length hands) in
    {
      curr_deck = new_deck;
      curr_card = card;
      curr_player = next_player;
      hands;
      human_index;
    }

  (* For a robot's turn, plays a robot card by drawing a random card from their
     hand and playing it *)
  let robot_turn (game : t) (player : int) : t =
    let curr_hand_lst = Hand.to_list (List.nth game.hands player) in
    let valid_cards = List.filter (fun c -> check_play game c) curr_hand_lst in
    let next_card =
      List.nth valid_cards (Random.int (List.length valid_cards))
    in
    play_card game next_card
      (Hand.play_card next_card (List.nth game.hands player))

  let handle_play (game : t) (is_human : bool) (card_input : string) : t =
    if is_human then
      if card_input = "" then raise (Invalid_argument "invalid card input")
      else
        let card = Card.to_card card_input in
        if check_play game card then
          let new_hand =
            Hand.play_card card (List.nth game.hands game.curr_player)
          in
          if Hand.to_list new_hand = [] then failwith "unimplemented"
            (* TODO: functionality for what to do when user won -> played last
               card - maybe add some field in t (ifWon?) to indicate curr_player
               has won*)
          else if List.length (Hand.to_list new_hand) = 1 then failwith "at uno"
            (* TODO: decide implementation for when user has one card left,
               maybe field in t (arr of those with one card) to indicate which
               players have uno? *)
          else play_card game card new_hand
        else raise (Invalid_argument "invalid move")
    else robot_turn game game.curr_player
end
