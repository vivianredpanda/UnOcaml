(** The signature of a deck of cards *)
module type Deck = sig
  type t
  (** Type representing the data in the deck. *)

  val to_list : t -> Card.Card.card list
  (** Convert a deck to a list of items. *)

  val of_list : Card.Card.card list -> t
  (** Convert a list of items into a deck. *)

  val draw : t -> Card.Card.card * t
  (** Draw a card from the deck. Returns the card drawn and the updated deck.
      Requires the given deck is non-empty. *)

  val deal : t -> t * Card.Card.card list
  (** Draws 7 cards from the deck. Returns the new deck and a list of 7 cards.
      Requires the deck contains at least 7 cards. *)

  val is_empty : t -> bool
  (** Returns whether or not the deck is empty. *)

  val reset : unit -> t
  (** Returns the default deck with 108 cards *)

  val size : t -> int
  (** Returns the number of items in the deck. *)
end

module Deck : Deck
(** Deck that consists of items of type card. *)
