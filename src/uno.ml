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

  val build : unit -> 'a t
  val play_card : 'a t -> 'b -> 'a t
  val play_round : 'a t -> string -> 'a t
end

module Game = struct
  type 'a t = {
    curr_deck : 'a Deck.t;
    curr_card : Card.card;
    curr_player : int; (* hands : Hand.Hand.t list; *)
  }

  (* Check if move is valid and return true if valid or false if invalid. *)
  let check_play : 'a t -> Card.card -> bool = failwith "unim"
  let build : unit -> 'a t = failwith "unim"
  let play_card : 'a t -> Card.card -> 'a t = failwith "unim"
  let play_round : 'a t -> string -> 'a t = failwith "unim"
end

(** robot code here to generate random card *)
(* store list of last seen cards here - to reference last played card *)
