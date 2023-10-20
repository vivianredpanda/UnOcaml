(* the cards available to draw from *)
(* includes actions like drawing from the deck, resetting the deck, dealing *)

(* smart deck & stupid/test deck *)

open Card

module type Deck = sig
  type t

  val to_list : t -> Card.card list
  val of_list : Card.card list -> t
  val remove : Card.card -> t -> t
  val draw : t -> Card.card * t
  val deal : t -> t * Card.card list
  val is_empty : t -> bool
  val reset : unit -> t
  val size : t -> int
end

module Deck : Deck = struct
  type t = Card.card list

  let to_list (deck : t) = deck
  let of_list (lst : Card.card list) = lst

  let rec remove (card : Card.card) (deck : t) =
    match deck with
    | [] -> raise (Invalid_argument "card not in deck")
    | h :: t -> if h = card then t else remove card t

  let draw (deck : t) =
    let card = List.nth deck (Random.int (List.length deck)) in
    (card, remove card deck)

  (** Given a list of cards [lst], a deck of cards [deck], and a counter [n],
      removes [n] cards from [deck]. Returns a pair of the updated deck and a
      list containing the removed cards and the elements of [lst]. Requires the
      size of [deck] is at least [n] and [n] >= 0. *)
  let rec deal_helper (lst : Card.card list) (deck : t) (n : int) =
    match n with
    | 0 -> (deck, lst)
    | _ ->
        let card, new_deck = draw deck in
        deal_helper (card :: lst) new_deck (n - 1)

  let deal (deck : t) = deal_helper [] deck 7

  let is_empty (deck : t) : bool =
    match to_list deck with
    | [] -> true
    | _ -> false

  let reset () : t = failwith "unim"
  let size (deck : t) = List.length deck
end
