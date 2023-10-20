(** The signature of a deck of cards *)
module type Deck = sig
  type 'a t
  (** Type representing the data in the deck. *)

  val to_list : 'a t -> 'a list
  (** Convert a deck to a list of items. *)

  val of_list : 'a list -> 'a t
  (** Convert a list of items into a deck. *)

  val draw : 'a t -> 'a * 'a t
  (** Draw a card from the deck. Returns the card drawn and the updated deck.
      Requires the given deck is non-empty. *)

  val deal : 'a t -> 'a t * 'a list
  (** Draws 7 cards from the deck. Returns the new deck and a list of 7 cards.
      Requires the deck contains at least 7 cards. *)

  val is_empty : 'a t -> bool
  (** Returns whether or not the deck is empty. *)

  val reset : unit -> 'a t
  (** Returns the default deck with 108 cards *)

  val size : 'a t -> int
  (** Returns the number of items in the deck. *)
end

module Deck : Deck
(** Deck that consists of items of type card. *)
