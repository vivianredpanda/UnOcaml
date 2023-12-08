(* putting down card - checking if chosen card is valid (hidden method in .ml)
   getting new card - returns what card you get - and remove that from deck
   to_list list of cards to hand *)

(** The signature of a Hand of 7 cards *)
module type Hand = sig
  type t
  (** Type representing the data in the hand *)

  val add_card : Card.Card.card -> t -> t
  (** Takes in a card and a hand, returns the original hand with the new card. *)

  val play_card : Card.Card.card -> t -> t
  (** Attempts to play a card from the current deck. Raises Invalid_argument if
      the given card does not exist in the current deck *)

  val of_list : Card.Card.card list -> t
  (** Takes in a list of cards and returns it as a Hand. *)

  val to_list : t -> Card.Card.card list
  (** Takes in a Hand of cards and returns it as a list of cards. *)

  val check_valid_card : Card.Card.card -> t -> bool
  (** Takes in a card and a hand and checks if the card is in the hand. *)
end

module Hand : Hand
(** Hand that consists of items of type card *)
