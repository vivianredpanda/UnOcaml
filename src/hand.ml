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
  type t

  val add_card : Card.card -> t -> t
  val play_card : Card.card -> t -> t
  val of_list : Card.card list -> t
  val to_list : t -> Card.card list
  val check_valid_card : Card.card -> t -> bool
end

module Hand : Hand = struct
  type t = Card.card list

  (* Checks that card c exists in h *)
  let check_valid_card (c : Card.card) (h : t) =
    match c with
    | Wildcard col ->
        List.exists (fun x -> (x = Card.(Wildcard Any)) || x = c) h
    | Wildcard4 col ->
        List.exists (fun x -> (x = Card.(Wildcard4 Any)) || x = c) h
    | _ -> List.exists (fun x -> x = c) h

  let add_card (c : Card.card) (h : t) : t = c :: h

  (* Returns [h] after removing one instance of the element [c] *)
  let rec remove_card (c : Card.card) (h : t) (acc : t) : t =
    let card =
      match c with
      | Wildcard color -> if List.mem c h then c else Card.(Wildcard Any)
      | Wildcard4 color -> if List.mem c h then c else Card.(Wildcard4 Any)
      | _ -> c
    in
    match h with
    | [] -> acc
    | h :: t -> if h = card then acc @ t else remove_card card t (h :: acc)

  let play_card (c : Card.card) (h : t) : t =
    if check_valid_card c h then remove_card c h []
    else raise (Invalid_argument "The card provided is not in your hand")

  let of_list (lst : Card.card list) : t = lst
  let to_list (h : t) : Card.card list = h
end
