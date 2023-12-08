(** The signature of a deck of cards *)
module type Deck = sig
  type t
  (** Type representing the data in the deck. *)

  val to_list : t -> Card.Card.card list
  (** Convert a deck to a list of items. *)

  val of_list : Card.Card.card list -> t
  (** Convert a list of items into a deck. *)

  val reset : unit -> t
  (** Returns the default deck with 108 cards *)

  val remove : Card.Card.card -> t -> t
  (** Remove a card from the given deck. Returns the updated deck. Raises
      Invalid_argument if card is not in the deck or deck is empty. *)

  val draw : t -> Card.Card.card * t
  (** Draw a card from the deck. Returns the card drawn and the updated deck.
      Raises Invalid_argument if the given deck is empty. *)

  val draw_n : t -> int -> t * Card.Card.card list
  (** Draw n cards from the deck. Returns a list of the cards drawn and the
      updated deck. If the given deck does not contain at least n cards, first
      draws as many cards from the deck as possible, then resets the deck and
      draws the rest. *)

  val deal : t -> t * Card.Card.card list
  (** Draws 7 cards from the deck. Returns the new deck and a list of 7 cards.
      Resets the deck and continues dealing if input deck has less than 7 cards. *)

  val is_empty : t -> bool
  (** Returns whether or not the deck is empty. *)

  val size : t -> int
  (** Returns the number of items in the deck. *)
end

module Deck : Deck
(** Deck that consists of items of type card. *)
