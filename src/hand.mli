open Deck

(* putting down card - checking if chosen card is valid (hidden method in .ml)
   getting new card - returns what card you get - and remove that from deck
   to_list list of cards to hand *)

(** The signature of a Hand of 7 cards *)
module type Hand = sig
  type 'a t
  (* Type representing the data in the hand *)

  val add_card : 'a -> 'a t -> 'a t
  (* Takes in a card and a hand, returns the original hand with the new card. *)

  val play_card : 'a -> 'a t -> 'a t
  (* Attempts to play a card from the current deck. Raises Invalid_argument if
     the given card does not exist in the current deck *)

  val of_list : 'a list -> 'a t
  (* Takes in a list of cards and returns it as a Hand. Raises Invalid_argument
     if the list does not contain 7 cards. *)

  val to_list : 'a t -> 'a list
  (* Takes in a Hand of cards and returns it as a list of cards. *)

  val check_valid_card : 'a -> 'a t -> bool
  (* Takes in a card and a hand and checks if the card is in the hand. *)
end

module Hand : Hand
(** Hand that consists of items of type card *)
