open Card
open Deck

(** The signature of a Hand of 7 cards *)
module type Hand = sig
  type t

  val add_card : Card.card -> t -> t
  val play_card : Card.card -> t -> t
  val of_list : Card.card list -> t
  val to_list : t -> Card.card list
  val check_valid_card : Card.card -> t -> bool
  val get_number : t -> int -> t
  val get_color : t -> Card.color -> t
  val get_skip : t -> Card.color -> t
  val get_reverse : t -> Card.color -> t
  val get_plus : t -> Card.color -> t
  val get_wild : t -> t
  val get_wild4 : t -> t
  val size : t -> int
end

(** Hand that consists of items of type card *)
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
  let rec remove_card (c : Card.card) (hand : t) (acc : t) : t =
    let card =
      match c with
      | Wildcard color -> if List.mem c hand then c else Card.(Wildcard Any)
      | Wildcard4 color -> if List.mem c hand then c else Card.(Wildcard4 Any)
      | _ -> c
    in
    match hand with
    | [] -> acc
    | h :: t -> if h = card then acc @ t else remove_card card t (h :: acc)

  let play_card (c : Card.card) (h : t) : t =
    if check_valid_card c h then remove_card c h []
    else raise (Invalid_argument "The card provided is not in your hand")

  let of_list (lst : Card.card list) : t = lst
  let to_list (h : t) : Card.card list = h

  let get_number (h : t) (num : int) : t =
    let lst =
      List.filter
        (fun c ->
          match c with
          | Card.Number (n, _) -> n = num
          | _ -> false)
        (to_list h)
    in
    of_list lst

  let get_color (h : t) (col : Card.color) : t =
    let lst =
      List.filter
        (fun c ->
          match c with
          | Card.Number (_, co) -> col = co
          | Card.Reverse co -> col = co
          | Card.Plus (_, co) -> col = co
          | Card.Skip co -> col = co
          | Card.Wildcard co -> col = co
          | Card.Wildcard4 co -> col = co)
        (to_list h)
    in
    of_list lst

  let get_skip (h : t) (col : Card.color) : t =
    let lst =
      List.filter
        (fun c ->
          match c with
          | Card.Skip co -> col = co
          | _ -> false)
        (to_list h)
    in
    of_list lst

  let get_reverse (h : t) (col : Card.color) : t =
    let lst =
      List.filter
        (fun c ->
          match c with
          | Card.Reverse co -> col = co
          | _ -> false)
        (to_list h)
    in
    of_list lst

  let get_plus (h : t) (col : Card.color) : t =
    let lst =
      List.filter
        (fun c ->
          match c with
          | Card.Plus (_, co) -> col = co
          | _ -> false)
        (to_list h)
    in
    of_list lst

  let get_wild (h : t) : t =
    let lst =
      List.filter
        (fun c ->
          match c with
          | Card.Wildcard _ -> true
          | _ -> false)
        (to_list h)
    in
    of_list lst

  let get_wild4 (h : t) : t =
    let lst =
      List.filter
        (fun c ->
          match c with
          | Card.Wildcard4 _ -> true
          | _ -> false)
        (to_list h)
    in
    of_list lst

  let size (lst : t) = List.length lst
end
