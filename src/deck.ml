(* the cards available to draw from *)
(* includes actions like drawing from the deck, resetting the deck, dealing *)

(* smart deck & stupid/test deck *)

open Card

module type Deck = sig
  type 'a t

  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val draw : 'a t -> 'a * 'a t
  val deal : 'a t -> 'a t * 'a list
  val empty : 'a t -> 'a t
  val size : 'a t -> int
end

module Deck : Deck = struct
  type 'a t = 'a list

  let to_list (deck : 'a t) = deck
  let of_list (lst : 'a list) = lst

  (** Remove a card [card] from the given deck [deck]. Returns the updated deck.
      Raises Invalid_argument if card is not in the deck or [deck] is empty. *)
  let rec remove (card : 'a) (deck : 'a t) =
    match deck with
    | [] -> raise (Invalid_argument "card not in deck")
    | h :: t -> if h = card then t else remove card t

  let draw (deck : 'a t) =
    let card = List.nth deck (Random.int (List.length deck)) in
    (card, remove card deck)

  (** Given a list of cards [lst], a deck of cards [deck], and a counter [n],
      removes [n] cards from [deck]. Returns a pair of the updated deck and a
      list containing the removed cards and the elements of [lst]. Requires the
      size of [deck] is at least [n] and [n] >= 0. *)
  let rec deal_helper (lst : 'a list) (deck : 'a t) (n : int) =
    match n with
    | 0 -> (deck, lst)
    | _ ->
        let card, new_deck = draw deck in
        deal_helper (card :: lst) new_deck (n - 1)

  let deal (deck : 'a t) = deal_helper [] deck 7
  let empty (deck : 'a t) = []
  let size (deck : 'a t) = List.length deck
end
