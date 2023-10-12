(* the cards available to draw from *)
(* includes actions like drawing from the deck, resetting the deck, dealing *)

(* smart deck & stupid/test deck *)

type color =
  | Red
  | Blue
  | Green
  | Yellow

type card =
  | Number of int * color
  | Reverse of color
  | Plus of int * color
  | Skip of color
  | Wildcard
  | Wildcard4

module type Deck = sig
  type 'a t

  val draw : 'a t -> 'a * 'a t
  val deal : 'a t -> 'a t
  val empty : 'a t -> 'a t
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end

module Deck : Deck = struct
  type 'a t = card list

  let draw = failwith "unimplemented"
  let deal = failwith "unimplemented"
  let empty = failwith "unimplemented"
  let size = failwith "unimplemented"
  let to_list = failwith "unimplemented"
  let of_list = failwith "unimplemented"
end
