open Hand
open Card
open Deck

(* game file *)

module type Game = sig
  type t

  type status =
    | Normal
    | Uno
    | Won

  val get_deck : t -> Deck.t
  val get_curr_card : t -> Card.card
  val get_curr_player : t -> int
  val get_hand : t -> int -> Hand.t
  val get_human_index : t -> int
  val get_curr_status : t -> string
  val get_prev_status : t -> string
  val hands_to_list : t -> Card.card list list
  val check_play : t -> Card.card -> bool
  val build : int -> t
  val play_card : t -> Card.card -> Hand.t -> t
  val handle_play : t -> bool -> string -> t
  val robot_turn : t -> int -> t
end

module Game = struct
  type status =
    | Normal
    | Uno
    | Won

  type t = {
    curr_deck : Deck.t;
    curr_card : Card.card;
    curr_player : int;
    hands : Hand.t list;
    human_index : int;
    statuses : status list;
  }

  let get_deck (game : t) : Deck.t = game.curr_deck
  let get_curr_card (game : t) : Card.card = game.curr_card
  let get_curr_player (game : t) : int = game.curr_player
  let get_human_index (game : t) : int = game.human_index

  (* todo: make this work for any n because game can have any number of players
     and doesnt have to be 4 player *)
  let get_hand (game : t) (player_num : int) : Hand.t =
    let hands = game.hands in
    match player_num with
    | 0 -> List.nth hands 0
    | 1 -> List.nth hands 1
    | 2 -> List.nth hands 2
    | 3 -> List.nth hands 3
    | _ -> failwith "invalid player number"

  let status_to_string stat =
    match stat with
    | Normal -> "Normal"
    | Uno -> "Uno"
    | Won -> "Won"

  let get_curr_status (game : t) : string =
    status_to_string (List.nth game.statuses game.curr_player)

  let get_prev_status (game : t) : string =
    let prev_idx = (game.curr_player - 1) mod List.length game.hands in
    status_to_string (List.nth game.statuses prev_idx)

  let hands_to_list (game : t) : Card.card list list =
    let rec to_list_list hands =
      match hands with
      | [] -> []
      | hand :: t -> Hand.to_list hand :: to_list_list t
    in
    to_list_list game.hands

  let check_play (game : t) (card : Card.card) : bool =
    match card with
    | Wildcard _ | Wildcard4 _ -> true
    | Skip color -> (
        Card.get_color game.curr_card = color
        ||
        match game.curr_card with
        | Skip _ -> true
        | _ -> false)
    | Reverse color -> (
        Card.get_color game.curr_card = color
        ||
        match game.curr_card with
        | Reverse _ -> true
        | _ -> false)
    | Number (num, color) ->
        if Card.get_color game.curr_card <> color then
          match Card.get_number game.curr_card with
          | Some n -> n = num
          | None -> false
        else true
    | Plus (num, color) ->
        if Card.get_color game.curr_card <> color then
          match game.curr_card with
          | Plus (num, _) -> begin
              match Card.get_number game.curr_card with
              | Some n -> n = num
              | None -> false
            end
          | _ -> false
        else true

  (* Build helper function. Takes in a deck [deck] to deal from, a list of hands
     [lst], and a number of hands to deal [n]. Deals [n] hands and adds each one
     to the input list. Returns the list of hands and the updated deck. *)
  let rec deal_hands deck lst n =
    match n with
    | 0 -> (lst, deck)
    | n ->
        let new_deck, hand = Deck.deal deck in
        deal_hands new_deck (Hand.of_list hand :: lst) (n - 1)

  (* Build helper function, sets the statuses of all the players initially to
     Normal by returning a list of the length of # players with each element as
     Normal. *)
  let rec init_statuses n = if n = 0 then [] else Normal :: init_statuses (n - 1)

  let build n =
    let start_deck = Deck.reset () in
    let starting_hands, dealt_deck = deal_hands start_deck [] n in
    let fst_card, deck = Deck.draw dealt_deck in
    {
      curr_deck = deck;
      curr_card = fst_card;
      curr_player = 0;
      hands = starting_hands;
      human_index = 0;
      statuses = init_statuses (List.length starting_hands);
    }

  (* Checks if the given card is a Reverse card *)
  let check_reverse (card : Card.card) =
    match card with
    | Reverse _ -> true
    | _ -> false

  (* Given a list [hands], an index of that list [player], and an element
     [new_hand] to add at that index, replaces the previous element at the index
     with [new_hand] and returns the updated list. *)
  let rec replace (hands : 'b list) (player : int) (new_hand : 'b) : Hand.t list
      =
    match hands with
    | [] -> []
    | h :: t ->
        if player = 0 then new_hand :: t
        else h :: replace t (player - 1) new_hand

  (* Given a card, a player index, and a total number of players, returns the
     index of the next player. Requires 0 < [player] < [n] - 1. *)
  let next_player (card : Card.card) (player : int) (n : int) : int =
    match card with
    | Number _ | Plus _ | Wildcard _ | Wildcard4 _ | Reverse _ ->
        if player = n - 1 then 0 else player + 1
    | Skip _ -> (player + 1) mod n

  (* Given a list of drawn cards and a hand to add them to, returns the new hand
     with all the cards added. *)
  let rec add_to_hand (cards : Card.card list) (hand : Hand.t) : Hand.t =
    match cards with
    | [] -> hand
    | card :: t -> add_to_hand t (Hand.add_card card hand)

  (* Given n number of cards to draw, a hand to add to, and a deck, draws n
     cards from the deck and adds them to the given hand, then returns the new
     deck and new hand. *)
  let rec draw_n_cards (n : int) (hand : Hand.t) (deck : Deck.t) :
      Deck.t * Hand.t =
    let new_deck, drawn_cards = Deck.draw_n deck n in
    let new_hand = add_to_hand drawn_cards hand in
    (new_deck, new_hand)

  let handle_draw (n : int) (hands : Hand.t list) (player : int) (hand : Hand.t)
      (deck : Deck.t) : Hand.t list * Deck.t =
    let new_deck, new_hand = draw_n_cards n hand deck in
    (replace hands player new_hand, new_deck)

  (* Given a game state, the index of the current player, the current player's
     hand after playing a card, and the card played, return the list of hands of
     all the players based on the card played, and the updated deck. *)
  let update_hands (game : t) (player : int) (new_hand : 'b) (card : Card.card)
      =
    let new_hands, new_deck =
      (replace game.hands player new_hand, game.curr_deck)
    in
    let next_idx = next_player card player (List.length new_hands) in
    let next_hand = List.nth new_hands next_idx in
    match card with
    | Number _ | Wildcard _ | Skip _ | Reverse _ -> (new_hands, new_deck)
    | Wildcard4 _ -> handle_draw 4 new_hands next_idx next_hand new_deck
    | Plus (n, _) -> handle_draw n new_hands next_idx next_hand new_deck

  let play_card (game : t) (card : Card.card) (new_hand : Hand.t) =
    let player = game.curr_player in
    let hands, new_deck = update_hands game player new_hand card in
    let curr_player_index, hands, human_index =
      if check_reverse card then
        ( List.length game.hands - player - 1,
          List.rev hands,
          List.length hands - 1 )
      else (player, hands, game.human_index)
    in
    let next_player = next_player card curr_player_index (List.length hands) in
    (* TODO: remove this print stuff later *)
    (* let rec hand_to_str (hnd : Card.card list) = match hnd with | [] -> "" |
       h :: t -> Card.string_of_card h ^ " " ^ hand_to_str t in print_endline
       ("after playing card " ^ Card.string_of_card card ^ " for new hand: " ^
       hand_to_str (Hand.to_list (List.nth hands curr_player_index))); *)
    {
      curr_deck = new_deck;
      curr_card = card;
      curr_player = next_player;
      hands;
      human_index;
      statuses = game.statuses;
    }

  (* Updates a certain player's status based on the number of cards they have
     left and returns the game state. *)
  let update_status (game : t) (player : int) (s : status) : t =
    let new_statuses =
      List.mapi (fun i x -> if i = player then s else x) game.statuses
    in
    {
      curr_deck = game.curr_deck;
      curr_card = game.curr_card;
      curr_player = game.curr_player;
      hands = game.hands;
      human_index = game.human_index;
      statuses = new_statuses;
    }

  let robot_smart_wildcard (game : t) (player : int) (card : Card.card) : t =
    (* do some math -> choose wildcard color based on most common *)
    play_card game card (Hand.play_card card (List.nth game.hands player))

  (* For a robot's turn, plays a robot card by drawing a random card from their
     hand and playing it *)
  let robot_turn (game : t) (player : int) : t =
    let curr_hand_lst = Hand.to_list (List.nth game.hands player) in
    let valid_cards = List.filter (fun c -> check_play game c) curr_hand_lst in
    let next_card =
      List.nth valid_cards (Random.int (List.length valid_cards))
    in
    (* TODO: make method to count type of card for each type (eg: # wildcards, #
       plus cards, etc.) *)
    (* based on that number of some type of card -> do some move *)
    (* normally just get rid of number cards *)
    (* also ai for reverse - check other players' number of cards *)
    (* same for skip or plus - we can prioritize playing a skip if estimate if
       estimate low for next player *)
    match next_card with
    | Number _ | Skip _ | Reverse _ | Plus _ ->
        play_card game next_card
          (Hand.play_card next_card (List.nth game.hands player))
    | Wildcard _ | Wildcard4 _ -> robot_smart_wildcard game player next_card

  (* TODO: add functionality to handle robot playing wildcards *)

  let handle_play (game : t) (is_human : bool) (card_input : string) : t =
    if is_human then
      if card_input = "" then raise (Invalid_argument "invalid card input")
      else if card_input = "Any Wildcard" then
        raise (Invalid_argument "need to specify wildcard color")
      else if card_input = "Any Wildcard4" then
        raise (Invalid_argument "need to specify wildcard4 color")
      else if card_input = "Draw card" then
        let new_hands, new_deck =
          handle_draw 1 game.hands game.curr_player
            (List.nth game.hands game.curr_player)
            game.curr_deck
        in
        {
          curr_deck = new_deck;
          curr_card = game.curr_card;
          curr_player = game.curr_player;
          hands = new_hands;
          human_index = game.human_index;
          statuses = game.statuses;
        }
      else if card_input = "Pass" then
        let next_idx = (game.curr_player + 1) mod List.length game.hands in
        {
          curr_deck = game.curr_deck;
          curr_card = game.curr_card;
          curr_player = next_idx;
          hands = game.hands;
          human_index = game.human_index;
          statuses = game.statuses;
        }
      else
        let card = Card.to_card card_input in
        (* TODO: look into potential bug - printing with Plus 2, Wildcard Plus,
           etc don't work - doesn't update the curr hand correctly *)
        if check_play game card then
          let new_hand =
            Hand.play_card card (List.nth game.hands game.curr_player)
          in
          (* TODO: also remove this print stuff later *)
          (* let rec hand_to_str (hnd : Card.card list) = match hnd with | [] ->
             "" | h :: t -> Card.string_of_card h ^ " " ^ hand_to_str t in
             print_endline ("test print of new_hand b4 play_card " ^ hand_to_str
             (Hand.to_list new_hand)); *)
          if Hand.to_list new_hand = [] then
            play_card (update_status game game.curr_player Won) card new_hand
          else if List.length (Hand.to_list new_hand) = 1 then
            play_card (update_status game game.curr_player Uno) card new_hand
          else play_card game card new_hand
        else raise (Invalid_argument "invalid move")
    else robot_turn game game.curr_player
end
