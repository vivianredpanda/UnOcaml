open Hand
open Card
open Deck

(* game file *)

(** 1. GAME PRINTING STUFF HERE 2. MAKE DEFAULT DECK - (RANDOMLY GRAB FROM DECK)
    OF 108 CARDS a. make reset function in deck & call that for a deck starter
    variable 3. deck draws 7 cards from the deck (list of cards) and that calls
    of_list to make a new hand 4. print instructions to begin play a. for each
    play: display the last played card (if no card, then draw a random card from
    the deck then user gets prompted to choose a card from their deck user gives
    input and we verify the input (print more stuff) *)

(* module Move : Move = struct (* let deck *) (* Deck.reset *) end *)

module type Game = sig
  type t

  val build : int -> t
  val play_card : t -> Card.card -> Hand.t -> t
  val play_round : t -> string -> t
end

module Game = struct
  (* UNIMPLEMENTED: stacking +2 or +4 *)
  type t = {
    curr_deck : Deck.t;
    curr_card : Card.card;
    curr_player : int;
    hands : Hand.t list;
  }

  (* Check if move is valid and return true if valid or false if invalid. *)
  let check_play game card =
    match game.curr_card with
    | Wildcard _ | Wildcard4 _ -> true
    | Skip color | Reverse color -> Card.get_color card = color
    | Number (num, color) | Plus (num, color) ->
        if Card.get_color card <> color then
          match Card.get_number card with
          | Some n -> n = num
          | None -> false
        else false

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
    }

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
    | Number _ | Plus _ | Wildcard _ | Wildcard4 _ ->
        if player = n - 1 then 0 else player + 1
    | _ -> failwith "Non-number card functionality unimplemented"
    | Skip _ ->
        if player = n - 1 then 1 else if player = n - 2 then 0 else player + 2
    | Reverse _ -> if player = 0 then n - 1 else player - 1
  (* To-do: Add non-number card functionality to determine the next_player when
     reverse or skip is played. *)

  (* Given a game state, the index of the current player, the current player's
     hand after playing a card, and the card played, return the list of hands of
     all the players based on the card played, and the updated deck. *)
  let update_hands (game : t) (player : int) (new_hand : 'b) (card : Card.card)
      =
    match card with
    | Number _ | Wildcard _ | Skip _ ->
        (replace game.hands player new_hand, game.curr_deck)
    | _ -> failwith "Non-number card functionality unimplemented"
  (* To-do: Add non-number card functionality to switch hands around when
     reverse is played, and add cards to the next player's deck if Plus/
     Wildcard4 is played. *)

  let play_card (game : t) (card : Card.card) (new_hand : Hand.t) =
    if check_play game card then
      let player = game.curr_player in
      let hands, new_deck = update_hands game player new_hand card in
      let next_player = next_player card player (List.length hands) in
      {
        curr_deck = new_deck;
        curr_card = card;
        curr_player = next_player;
        hands;
      }
    else raise (Invalid_argument "invalid move")

  let play_round : t -> string -> t = failwith "unim"
end

(** robot code here to generate random card *)
(* store list of last seen cards here - to reference last played card *)
