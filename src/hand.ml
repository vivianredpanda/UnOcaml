(* represents the cards that one player currently has *)
(* includes checking valid cards, checking for one card left, printing the cards
   available, adding cards, list to hand*)
(* checking if card is valid is a hidden method in here *)
(* putting down card - checking if chosen card is valid (hidden method in .ml)
   getting new card - returns what card you get - and remove that from deck
   to_list list of cards to hand *)
open Card
open Deck

module type Hand = sig
  type 'a t

  val add_card : 'a -> 'a t -> 'a t
  val play_card : 'a -> 'a t -> 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
end

module Hand : Hand = struct
  type 'a t = 'a list

  (* Checks that card c exists in h *)
  let rec check_valid_card (c : 'a) (h : 'a t) : bool =
    match h with
    | [] -> false
    | h :: t -> if h = c then true else check_valid_card c t

  let add_card (c : 'a) (h : 'a t) : 'a t = c :: h

  let play_card (c : 'a) (h : 'a t) : 'a t =
    if check_valid_card c h then List.filter (fun x -> x <> c) h
    else raise (Invalid_argument "The card provided is not in your hand")

  let of_list (lst : 'a list) : 'a t = lst
  let to_list (h : 'a t) : 'a list = h
end
