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
  let draw (deck : 'a t) = failwith "unimplemented"
  let deal (deck : 'a t) = failwith "unimplemented"
  let empty (deck : 'a t) = []
  let size (deck : 'a t) = List.length deck
end
