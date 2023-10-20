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
  type 'a t

  val build : int -> 'a t
  val play_card : 'a t -> 'b -> 'a t
  val play_round : 'a t -> string -> 'a t
end

module Game = struct
  type 'a t = {
    curr_deck : 'a Deck.t;
    curr_card : Card.card;
    curr_player : int;
    hands : 'a list list;
  }

  (* Check if move is valid and return true if valid or false if invalid. *)
  let check_play : 'a t -> Card.card -> bool = failwith "unim"

  (* Build helper function. Takes in a deck [deck] to deal from, a list of hands
     [lst], and a number of hands to deal [n]. Deals [n] hands and adds each one
     to the input list. Returns the list of hands and the updated deck. *)
  let rec deal_hands deck lst n =
    match n with
    | 0 -> (lst, deck)
    | n ->
        let new_deck, hand = Deck.deal deck in
        deal_hands new_deck (hand :: lst) (n - 1)

  let build n =
    let start_deck = Deck.reset () in
    let hands, dealt_deck = deal_hands start_deck [] n in
    let fst_card, deck = Deck.draw dealt_deck in
    { curr_deck = deck; curr_card = fst_card; curr_player = 0; hands }

  let play_card : 'a t -> Card.card -> 'a t = failwith "unim"
  let play_round : 'a t -> string -> 'a t = failwith "unim"
end

(** robot code here to generate random card *)
(* store list of last seen cards here - to reference last played card *)
