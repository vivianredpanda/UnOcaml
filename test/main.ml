(* Our approach to testing was to create unit tests for all of the functions
   that were possible to test. We created some of these unit tests along the
   way, when coding a new function, but we also went back to add more tests
   later. In the test suite, we omitted testing any functionality that involved
   randomness. For example, we were unable to fully test Uno.play_card because
   it involves playing a card from a hand on top of a randomly drawn card from
   the deck. For any functionality we could not write unit tests for, we tested
   in the terminal by playing the game. We also added a 'debug' option to use
   when playing the game to be able to see all the cards for the other players
   to check that the background players are playing correctly. We believe our
   test suite demonstrates correctness of our system because we used Bisect to
   check our coverage. The card, deck, and hand modules all had over 90%
   coverage, and although Uno.ml did not have very high coverage, it was
   thoroughly tested in terminal. *)

open OUnit2
open Unocaml
open Deck
open Card
open Hand
open Uno

let cmp_bag_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  Printf.sprintf "[%s]" (pp_elts lst)

let pp_string s = "\"" ^ s ^ "\""
let pp_card c = pp_string (Card.string_of_card c)

let pp_bool b =
  match b with
  | true -> "true"
  | false -> "false"

let unused_wildcard = Card.to_card "Any Wildcard"
let used_wildcard = Card.to_card "Blue Wildcard"
let unused_wildcard4 = Card.to_card "Any Wildcard4"
let used_wildcard4 = Card.to_card "Blue Wildcard4"
let number_card = Card.to_card "Green 5"
let reverse_card = Card.to_card "Yellow Reverse"
let skip_card = Card.to_card "Red Skip"
let plus2_card = Card.to_card "Blue Plus 2"
let plus4_card = Card.to_card "Red Plus 4"

let option_value o =
  match o with
  | Some v -> v
  | None -> -1

let get_color_test out in1 _ =
  assert_equal ~printer:Card.string_of_color
    ~msg:("function: get_color\ninputs: %s %s" ^ Card.string_of_card in1)
    out (Card.get_color in1)

let get_number_test out in1 _ =
  assert_equal ~printer:string_of_int
    ~msg:("function: get_number\ninputs: %s %s" ^ Card.string_of_card in1)
    out
    (option_value (Card.get_number in1))

let to_color_test name out in1 =
  name >:: fun _ ->
  assert_equal ~printer:pp_string out
    (in1 |> Card.to_color |> Card.string_of_color)

let to_color_failure_test color _ =
  let exn = Failure "..." in
  assert_raises ~msg:("function: to_color\ninput: %s" ^ color) exn (fun () ->
      try Card.to_color color with Failure _ -> raise exn)

let to_card_color_test name string_color string_type =
  name >:: fun _ ->
  let c = Card.to_card (string_color ^ " " ^ string_type) in
  assert_equal ~printer:pp_string
    (String.lowercase_ascii string_color)
    (c |> Card.get_color |> Card.string_of_color |> String.lowercase_ascii)

let to_card_type_test name string_color string_num =
  name >:: fun _ ->
  let c = Card.to_card (string_color ^ " " ^ string_num) in
  assert_equal ~printer:pp_string string_num
    (c |> Card.get_number |> option_value |> string_of_int)

let string_of_card_test name string =
  name >:: fun _ ->
  assert_equal ~printer:pp_string string
    (string |> Card.to_card |> Card.string_of_card)

let card_tests =
  [
    "get_color of Unused Wildcard"
    >:: get_color_test (Card.to_color "Any") unused_wildcard;
    "get_color of Used Wildcard"
    >:: get_color_test (Card.to_color "Blue") used_wildcard;
    "get_color of Unused Wildcard4"
    >:: get_color_test (Card.to_color "Any") unused_wildcard4;
    "get_color of Used Wildcard4"
    >:: get_color_test (Card.to_color "Blue") used_wildcard4;
    "get_color of Number Card"
    >:: get_color_test (Card.to_color "Green") number_card;
    "get_color of Reverse"
    >:: get_color_test (Card.to_color "Yellow") reverse_card;
    "get_color of Skip" >:: get_color_test (Card.to_color "Red") skip_card;
    "get_color of Plus Two" >:: get_color_test (Card.to_color "Blue") plus2_card;
    "get_color of Plus Four" >:: get_color_test (Card.to_color "Red") plus4_card;
    "get_number of Unused Wildcard" >:: get_number_test (-1) unused_wildcard;
    "get_number of Used Wildcard" >:: get_number_test (-1) used_wildcard;
    "get_number of Unused Wildcard4" >:: get_number_test (-1) unused_wildcard4;
    "get_number of Used Wildcard4" >:: get_number_test (-1) used_wildcard4;
    "get_number of Number Card" >:: get_number_test 5 number_card;
    "get_number of Reverse" >:: get_number_test (-1) reverse_card;
    "get_number of Skip" >:: get_number_test (-1) skip_card;
    "get_number of Plus Two" >:: get_number_test 2 plus2_card;
    "get_number of Plus Four" >:: get_number_test 4 plus4_card;
    to_color_test "to_color Red" "Red" "red";
    to_color_test "to_color Green" "Green" "Green";
    to_color_test "to_color Yellow" "Yellow" "yeLLOW";
    to_color_test "to_color Blue" "Blue" "blue";
    to_color_test "to_color Any" "Any" "anY";
    "to_color invalid string" >:: to_color_failure_test "orange";
    to_card_color_test "to_card red card" "red" "Wildcard";
    to_card_color_test "to_card any card" "any" "Wildcard4";
    to_card_color_test "to_card yellow card" "yellow" "Reverse";
    to_card_color_test "to_card blue card" "Blue" "Skip";
    to_card_color_test "to_card green card" "Green" "Plus 2";
    to_card_type_test "to_card yellow 4" "yellow" "4";
    to_card_type_test "to_card red 8" "Red" "8";
    to_card_type_test "to_card green 3" "Green" "3";
    string_of_card_test "string_of_card Yellow 4" "Yellow 4";
    string_of_card_test "string_of_card Any Wildcard" "Any Wildcard";
    string_of_card_test "string_of_card green Reverse" "Green Reverse";
    string_of_card_test "string_of_card red plus 2" "Red Plus 2";
  ]

let deck1 =
  Deck.of_list
    [
      reverse_card;
      skip_card;
      plus2_card;
      unused_wildcard;
      number_card;
      number_card;
      unused_wildcard4;
    ]

let deck2 = Deck.reset ()
let deck3 = Deck.of_list [ number_card ]
let deck4 = Deck.of_list [ number_card; number_card; number_card; number_card ]

let deck5 =
  Deck.of_list
    [
      number_card;
      Card.to_card "Red 6";
      Card.to_card "Yellow 7";
      Card.to_card "Blue 8";
      Card.to_card "Green 9";
      Card.to_card "Red 4";
      Card.to_card "Yellow 3";
    ]

let deck_empty = Deck.of_list []

let remove_test out in1 in2 _ =
  assert_equal
    ~printer:(pp_list Card.string_of_card)
    ~msg:
      ("function: remove\ninputs: %s %s" ^ Card.string_of_card in1
      ^ pp_list Card.string_of_card (Deck.to_list in2))
    out
    (Deck.remove in1 in2 |> Deck.to_list)

let remove_invalid_arg_test in1 in2 _ =
  let exn = Invalid_argument "..." in
  assert_raises
    ~msg:
      ("function: remove\ninput: %s %s" ^ Card.string_of_card in1
      ^ pp_list Card.string_of_card (Deck.to_list in2))
    exn
    (fun () -> try Deck.remove in1 in2 with Invalid_argument _ -> raise exn)

let deal_test out in1 _ =
  assert_equal ~printer:string_of_int
    ~msg:
      ("function: deal\ninputs: %s %s"
      ^ pp_list Card.string_of_card (Deck.to_list in1))
    out
    (Deck.deal in1 |> fst |> Deck.to_list |> List.length)

let int_test name out in1 =
  name >:: fun _ -> assert_equal ~printer:string_of_int out in1

let draw_test name out deck =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out (Deck.size (snd (Deck.draw deck)))

let draw_invalid_arg_test deck _ =
  let exn = Invalid_argument "..." in
  assert_raises
    ~msg:
      ("function: draw\ninput: %s"
      ^ pp_list Card.string_of_card (Deck.to_list deck))
    exn
    (fun () -> try Deck.draw deck with Invalid_argument _ -> raise exn)

let draw_n_deck_test name out deck n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out (Deck.size (fst (Deck.draw_n deck n)))

let draw_n_hand_test name out deck n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out
    (List.length (snd (Deck.draw_n deck n)))

let is_empty_test name out deck =
  name >:: fun _ -> assert_equal ~printer:pp_bool out (Deck.is_empty deck)

let deck_tests =
  [
    "remove first card"
    >:: remove_test
          [
            skip_card;
            plus2_card;
            unused_wildcard;
            number_card;
            number_card;
            unused_wildcard4;
          ]
          reverse_card deck1;
    "remove card within list"
    >:: remove_test
          [
            reverse_card;
            skip_card;
            unused_wildcard;
            number_card;
            number_card;
            unused_wildcard4;
          ]
          plus2_card deck1;
    "remove card that contains multiple of same value in list"
    >:: remove_test
          [
            reverse_card;
            skip_card;
            plus2_card;
            unused_wildcard;
            number_card;
            unused_wildcard4;
          ]
          number_card deck1;
    "remove card that does not exist in list"
    >:: remove_invalid_arg_test plus4_card deck1;
    "remove card that from an empty list"
    >:: remove_invalid_arg_test plus4_card deck_empty;
    "deal cards from beginning deck" >:: deal_test 101 deck2;
    "deal cards from small deck" >:: deal_test 0 deck1;
    "deal cards from deck size less than 7"
    >:: deal_test 107 (snd (Deck.draw deck1));
    "deal cards from empty deck" >:: deal_test 101 deck_empty;
    int_test "draw card from deck of one card" 5
      (option_value (Card.get_number (fst (Deck.draw deck3))));
    draw_test "return deck after drawing from deck of one card" 0 deck3;
    draw_test "draw from full deck" 107 deck2;
    "draw from empty deck" >:: draw_invalid_arg_test deck_empty;
    draw_n_deck_test "return deck after drawing 0 cards" 108 deck2 0;
    draw_n_hand_test "return list of cards after drawing 0 cards" 0 deck2 0;
    draw_n_deck_test "return deck after drawing from small deck" 0 deck4 4;
    draw_n_hand_test "return list of cards after drawing from small deck" 4
      deck4 4;
    draw_n_deck_test "return deck after drawing from full deck" 98 deck2 10;
    draw_n_hand_test "return list of cards after drawing from full deck" 10
      deck2 10;
    int_test "draw_n draw one card from deck" 5
      (option_value (Card.get_number (List.hd (snd (Deck.draw_n deck4 1)))));
    is_empty_test "is_empty empty deck" true deck_empty;
    is_empty_test "is_empty 1-element deck" false deck3;
    is_empty_test "is_empty full deck" false deck1;
  ]

let hand1 =
  Hand.of_list
    [
      reverse_card;
      skip_card;
      plus2_card;
      unused_wildcard;
      Card.to_card "Green 8";
      number_card;
      Card.to_card "Red 5";
      plus2_card;
      unused_wildcard4;
    ]

let check_valid_card_test out in1 in2 _ =
  assert_equal ~printer:string_of_bool
    ~msg:
      ("function: remove\ninputs: %s %s" ^ Card.string_of_card in1
      ^ pp_list Card.string_of_card (Hand.to_list in2))
    out
    (Hand.check_valid_card in1 in2)

let add_card_test out in1 in2 _ =
  assert_equal
    ~printer:(pp_list Card.string_of_card)
    ~msg:
      ("function: remove\ninputs: %s %s" ^ Card.string_of_card in1
      ^ pp_list Card.string_of_card (Hand.to_list in2))
    out
    (Hand.add_card in1 in2 |> Hand.to_list)

let play_card_test out in1 in2 _ =
  assert_equal ~cmp:cmp_bag_like_lists
    ~printer:(pp_list Card.string_of_card)
    ~msg:
      ("function: remove\ninputs: %s %s" ^ Card.string_of_card in1
      ^ pp_list Card.string_of_card (Hand.to_list in2))
    out
    (Hand.play_card in1 in2 |> Hand.to_list)

let play_card_invalid_arg_test in1 in2 _ =
  let exn = Invalid_argument "..." in
  assert_raises
    ~msg:
      ("function: remove\ninput: %s %s" ^ Card.string_of_card in1
      ^ pp_list Card.string_of_card (Hand.to_list in2))
    exn
    (fun () ->
      try Hand.play_card in1 in2 with Invalid_argument _ -> raise exn)

let hand_tests =
  [
    "check_valid_card for existing card in the deck"
    >:: check_valid_card_test true plus2_card hand1;
    "check_valid_card for similar non-existing card in the deck"
    >:: check_valid_card_test false (Card.to_card "Red Plus 2") hand1;
    "check_valid_card for used wildcard vs unused wildcard"
    >:: check_valid_card_test true (Card.to_card "Blue Wildcard") hand1;
    "check_valid_card for any wildcard4"
    >:: check_valid_card_test true (Card.to_card "Red Wildcard4") hand1;
    "add_card random number card"
    >:: add_card_test
          [
            Card.to_card "Yellow 0";
            reverse_card;
            skip_card;
            plus2_card;
            unused_wildcard;
            Card.to_card "Green 8";
            number_card;
            Card.to_card "Red 5";
            plus2_card;
            unused_wildcard4;
          ]
          (Card.to_card "Yellow 0") hand1;
    "add_card already existing card"
    >:: add_card_test
          [
            reverse_card;
            reverse_card;
            skip_card;
            plus2_card;
            unused_wildcard;
            Card.to_card "Green 8";
            number_card;
            Card.to_card "Red 5";
            plus2_card;
            unused_wildcard4;
          ]
          reverse_card hand1;
    "play_card first card in list"
    >:: play_card_test
          [
            skip_card;
            plus2_card;
            unused_wildcard;
            Card.to_card "Green 8";
            number_card;
            Card.to_card "Red 5";
            plus2_card;
            unused_wildcard4;
          ]
          reverse_card hand1;
    "play_card last card in list"
    >:: play_card_test
          [
            reverse_card;
            skip_card;
            plus2_card;
            unused_wildcard;
            Card.to_card "Green 8";
            number_card;
            Card.to_card "Red 5";
            plus2_card;
          ]
          (Card.to_card "Blue Wildcard4")
          hand1;
    "play_card multiple same card in list"
    >:: play_card_test
          [
            reverse_card;
            skip_card;
            unused_wildcard;
            Card.to_card "Green 8";
            number_card;
            Card.to_card "Red 5";
            plus2_card;
            unused_wildcard4;
          ]
          plus2_card hand1;
    "play_card used wildcard in list"
    >:: play_card_test
          [
            reverse_card;
            skip_card;
            plus2_card;
            unused_wildcard;
            Card.to_card "Green 8";
            number_card;
            Card.to_card "Red 5";
            plus2_card;
            unused_wildcard4;
          ]
          (Card.to_card "Green Wildcard")
          (Hand.add_card (Card.to_card "Green Wildcard") hand1);
    "play_card a wildcard"
    >:: play_card_test
          [ Card.to_card "Yellow Reverse" ]
          (Card.to_card "Yellow Wildcard4")
          (Hand.of_list
             [ Card.to_card "Any Wildcard4"; Card.to_card "Yellow Reverse" ]);
    "play_card unused wildcard in list with used wildcard"
    >:: play_card_test
          [
            reverse_card;
            skip_card;
            plus2_card;
            Card.to_card "Yellow Wildcard";
            Card.to_card "Green 8";
            number_card;
            Card.to_card "Red 5";
            plus2_card;
            unused_wildcard4;
          ]
          (Card.to_card "Red Wildcard")
          (Hand.add_card (Card.to_card "Yellow Wildcard") hand1);
    "play_card non-existing card in list"
    >:: play_card_invalid_arg_test (Card.to_card "Green Skip") hand1;
  ]

let build_deck_test name out n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out
    (Game.build n |> Game.get_deck |> Deck.size)

let build_player_test name out n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out (Game.build n |> Game.get_curr_player)

let build_hands_test name out n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out
    (Game.build n |> Game.hands_to_list |> List.length)

let play_card_test name out n f1 f2 f3 =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out
    ((let game = Game.build n in
      let hand = Game.get_hand game 0 in
      let card, new_hand = Deck.draw (hand |> Hand.to_list |> Deck.of_list) in
      Game.play_card game card (new_hand |> Deck.to_list |> Hand.of_list))
    |> f1 |> f2 |> f3)

let get_hand_failure_test in1 in2 _ =
  let exn = Invalid_argument "..." in
  assert_raises
    ~msg:
      (Printf.sprintf "function: get_hand\ninput: %s %s" (string_of_int in1)
         (string_of_int in2))
    exn
    (fun () ->
      try Game.get_hand (Game.build in1) in2
      with Invalid_argument _ -> raise exn)

let get_hand_test name out n p =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:string_of_int out
    (Game.get_hand game p |> Hand.to_list |> List.length)

let get_status_test name out n player =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:pp_string out (Game.get_status game player)

let get_status_invalid_arg_test n player _ =
  let exn = Invalid_argument "..." in
  let game = Game.build n in
  assert_raises
    ~msg:
      (Printf.sprintf "function: get_status\ninput: %s %s" (string_of_int n)
         (string_of_int player))
    exn
    (fun () ->
      try Game.get_status game player with Invalid_argument _ -> raise exn)

let get_curr_status_test name out n =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:pp_string out (Game.get_curr_status game)

let get_curr_status_invalid_arg_test n _ =
  let exn = Invalid_argument "..." in
  let game = Game.build n in
  assert_raises
    ~msg:
      (Printf.sprintf "function: get_curr_status\ninput: %s" (string_of_int n))
    exn
    (fun () ->
      try Game.get_curr_status game with Invalid_argument _ -> raise exn)

let get_prev_status_test name out n =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:pp_string out (Game.get_prev_status game)

let get_prev_status_invalid_arg_test n _ =
  let exn = Invalid_argument "..." in
  let game = Game.build n in
  assert_raises
    ~msg:
      (Printf.sprintf "function: get_prev_status\ninput: %s" (string_of_int n))
    exn
    (fun () ->
      try Game.get_prev_status game with Invalid_argument _ -> raise exn)

let hands_to_list_test name n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int n
    (Game.build n |> Game.hands_to_list |> List.length)

let check_play_test name out n card =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:pp_bool out (Game.check_play game card)

let get_human_index_test name out n =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:string_of_int out (Game.get_human_index game)

let get_human_index_invalid_arg_test n _ =
  let exn = Invalid_argument "..." in
  let game = Game.build n in
  assert_raises
    ~msg:
      (Printf.sprintf "function: get_human_index\ninput: %s" (string_of_int n))
    exn
    (fun () ->
      try Game.get_human_index game with Invalid_argument _ -> raise exn)

let handle_play_invalid_arg_test n is_human card_input _ =
  let exn = Invalid_argument "..." in
  let game = Game.build n in
  assert_raises
    ~msg:
      (Printf.sprintf "function: handle_play\ninput: %s %s %s" (string_of_int n)
         (string_of_bool is_human) card_input)
    exn
    (fun () ->
      try Game.handle_play game is_human card_input
      with Invalid_argument _ -> raise exn)

let uno_tests =
  [
    build_deck_test "build 4 removes 4*7 + 1 cards from starter deck" 79 4;
    build_deck_test "build 2 removes 2*7 + 1 cards from starter deck" 93 2;
    build_deck_test "build 0 removes 0 + 1 cards from starter deck" 107 0;
    build_player_test "build 0 defaults to player 0 as starting player" 0 0;
    build_player_test "build 2 defaults to player 0 as starting player" 0 2;
    build_hands_test "build 0 creates 0 hands" 0 0;
    build_hands_test "build 1 creates 1 hand" 1 1;
    build_hands_test "build 3 creates 3 hands" 3 3;
    "get hand of player index 1 in a one player game"
    >:: get_hand_failure_test 1 1;
    "get hand of player 4 in a two player game" >:: get_hand_failure_test 2 4;
    get_hand_test "get_hand of player in newly dealt 4-player game" 7 4 1;
    get_hand_test "get_hand of player in newly dealt 1-player game" 7 1 0;
    get_curr_status_test "get status of newly dealt game with 3 players"
      "Normal" 3;
    get_curr_status_test "get status of newly dealt game with 1 player" "Normal"
      1;
    "get_status of 0 player game" >:: get_curr_status_invalid_arg_test 0;
    get_prev_status_test "get prev status of newly dealt game with 3 players"
      "Normal" 3;
    get_prev_status_test "get prev status of newly dealt game with 2 players"
      "Normal" 1;
    "get_prev_status of 0 player game" >:: get_prev_status_invalid_arg_test 0;
    hands_to_list_test "hands_to_list has 4 hands for a 4 player game" 4;
    hands_to_list_test "hands_to_list has 0 hands for a 0 player game" 0;
    hands_to_list_test "hands_to_list has 1 hand for a 1 player game" 1;
    play_card_test "play_card of first card does not change number of players" 3
      3 Game.hands_to_list List.length (( + ) 0);
    check_play_test "playing wildcard always valid" true 4
      (Card.to_card "Any Wildcard");
    check_play_test "playing wildcard4 always valid" true 3
      (Card.to_card "Yellow Wildcard4");
    get_human_index_test "get_human_index new game with 1 player" 0 1;
    get_human_index_test "get_human_index new game with 2 players" 0 2;
    get_human_index_test "get_human_index new game with 4 player" 0 4;
    get_status_test "get_status 1st player in 1 player game" "Normal" 1 0;
    get_status_test "get_status 1st player in 4 player game" "Normal" 4 0;
    get_status_test "get_status 2nd player in 4 player game" "Normal" 4 2;
    get_status_test "get_status 4th player in 4 player game" "Normal" 4 3;
    "get_status negative player index" >:: get_status_invalid_arg_test 1 (-1);
    "get_status of player 1 in a 0 player game"
    >:: get_status_invalid_arg_test 0 0;
    "get_status of player 4 in a 3 player game"
    >:: get_status_invalid_arg_test 3 3;
    "get_status of player 6 in a 3 player game"
    >:: get_status_invalid_arg_test 3 5;
    "get_human_index of 0 player game" >:: get_human_index_invalid_arg_test 0;
    "handle_play of human player with empty card input"
    >:: handle_play_invalid_arg_test 3 true "";
    "handle_play of human player with Any Wildcard input"
    >:: handle_play_invalid_arg_test 3 true "Any Wildcard";
    "handle_play of human player with Any Wildcard4 input"
    >:: handle_play_invalid_arg_test 3 true "Any Wildcard4";
  ]

let tests =
  "unocaml test suite"
  >::: List.flatten [ card_tests; deck_tests; hand_tests; uno_tests ]

let _ = run_test_tt_main tests
