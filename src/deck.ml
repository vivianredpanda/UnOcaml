open Card

(** The signature of a deck of cards *)
module type Deck = sig
  type t

  val to_list : t -> Card.card list
  val of_list : Card.card list -> t
  val reset : unit -> t
  val remove : Card.card -> t -> t
  val draw : t -> Card.card * t
  val draw_n : t -> int -> t * Card.card list
  val deal : t -> t * Card.card list
  val is_empty : t -> bool
  val size : t -> int
end

(** Deck that consists of items of type card. *)
module Deck : Deck = struct
  type t = Card.card list

  let to_list (deck : t) = deck
  let of_list (lst : Card.card list) = lst

  (* Creates [num] number of cards of the given color [color] with the string
     version of the card type [card_type] and value [num] except when num < 0.
     Returns [list] of all of the created cards *)
  let rec create_given_cards (color : string) (card_type : string) (num : int)
      (value : int) (list : Card.card list) : Card.card list =
    let card_to_add =
      if value < 0 then Card.to_card (color ^ " " ^ card_type)
      else Card.to_card (color ^ " " ^ card_type ^ " " ^ string_of_int value)
    in
    match num with
    | 0 -> list
    | _ ->
        create_given_cards color card_type (num - 1) value (card_to_add :: list)

  (* Creates [value]*2 + 1 cards of type Number with the given color [color]
     such that there is one card of value 0 and 2 cards each of value 1-n.
     Returns [list] of all of the created cards *)
  let rec create_num_cards (color : string) (value : int)
      (list : Card.card list) : Card.card list =
    match value with
    | 0 -> create_given_cards color "" 1 value list
    | _ ->
        create_num_cards color (value - 1)
          (create_given_cards color "" 2 value list)

  (* Creates 19 cards of type Number of each color such that there is 1 card of
     value 0 and 2 cards of value 1-9 for each color, 2 Skip of each color, 2
     Reverse cards of each color, two Draw cards of each color, 4 Wildcards, and
     4 Wildcard4 *)
  let reset () : t =
    create_num_cards "Blue" 9 []
    |> create_num_cards "Green" 9
    |> create_num_cards "Yellow" 9
    |> create_num_cards "Red" 9
    |> create_given_cards "Blue" "Skip" 2 (-1)
    |> create_given_cards "Green" "Skip" 2 (-1)
    |> create_given_cards "Yellow" "Skip" 2 (-1)
    |> create_given_cards "Red" "Skip" 2 (-1)
    |> create_given_cards "Blue" "Reverse" 2 (-1)
    |> create_given_cards "Green" "Reverse" 2 (-1)
    |> create_given_cards "Yellow" "Reverse" 2 (-1)
    |> create_given_cards "Red" "Reverse" 2 (-1)
    |> create_given_cards "Blue" "Plus" 2 2
    |> create_given_cards "Green" "Plus" 2 2
    |> create_given_cards "Yellow" "Plus" 2 2
    |> create_given_cards "Red" "Plus" 2 2
    |> create_given_cards "Any" "Wildcard" 4 (-1)
    |> create_given_cards "Any" "Wildcard4" 4 (-1)

  let rec remove (card : Card.card) (deck : t) =
    match deck with
    | [] -> raise (Invalid_argument "card not in deck")
    | h :: t -> if h = card then t else h :: remove card t

  let draw (deck : t) =
    match deck with
    | [] -> raise (Invalid_argument "empty input deck")
    | deck ->
        Random.self_init ();
        let card = List.nth deck (Random.int (List.length deck)) in
        (card, remove card deck)

  (** Given a list of cards [lst], a deck of cards [deck], and a counter [n],
      removes [n] cards from [deck]. Returns a pair of the updated deck and a
      list containing the removed cards and the elements of [lst]. Resets the
      deck if the input deck didn't have enough cards. Requires [n] >= 0. *)
  let rec deal_helper (lst : Card.card list) (deck : t) (n : int) =
    match n with
    | 0 -> (deck, lst)
    | _ -> (
        match deck with
        | [] ->
            let card, new_deck = draw (reset ()) in
            deal_helper (card :: lst) new_deck (n - 1)
        | _ ->
            let card, new_deck = draw deck in
            deal_helper (card :: lst) new_deck (n - 1))

  let draw_n (deck : t) (n : int) = deal_helper [] deck n
  let deal (deck : t) = draw_n deck 7

  let is_empty (deck : t) : bool =
    match to_list deck with
    | [] -> true
    | _ -> false

  let size (deck : t) = List.length deck
end
