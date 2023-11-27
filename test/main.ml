(* TEST PLAN describing your approach to testing: what you tested, anything you
   omitted testingm why we believe our test suite demonstrates the correctness
   of our system *)
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

let draw_n_deck_test name out deck n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out (Deck.size (fst (Deck.draw_n deck n)))

let draw_n_hand_test name out deck n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int out
    (List.length (snd (Deck.draw_n deck n)))

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
    "deal cards from beginning deck" >:: deal_test 101 deck2;
    "deal cards from small deck" >:: deal_test 0 deck1;
    "deal cards from deck size less than 7"
    >:: deal_test 107 (snd (Deck.draw deck1));
    int_test "draw card from deck of one card" 5
      (option_value (Card.get_number (fst (Deck.draw deck3))));
    draw_test "return deck after drawing from deck of one card" 0 deck3;
    draw_test "draw from full deck" 107 deck2;
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
  let exn = Failure "..." in
  assert_raises
    ~msg:
      (Printf.sprintf "function: get_hand\ninput: %s %s" (string_of_int in1)
         (string_of_int in2))
    exn
    (fun () ->
      try Game.get_hand (Game.build in1) in2 with Failure _ -> raise exn)

let get_hand_test name out n p =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:string_of_int out
    (Game.get_hand game p |> Hand.to_list |> List.length)

let get_curr_status_test name out n =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:pp_string out (Game.get_curr_status game)

let get_prev_status_test name out n =
  name >:: fun _ ->
  let game = Game.build n in
  assert_equal ~printer:pp_string out (Game.get_prev_status game)

let hands_to_list_test name n =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int n
    (Game.build n |> Game.hands_to_list |> List.length)

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
    get_prev_status_test "get prev status of newly dealt game with 3 players"
      "Normal" 3;
    get_prev_status_test "get prev status of newly dealt game with 2 players"
      "Normal" 1;
    hands_to_list_test "hands_to_list has 4 hands for a 4 player game" 4;
    hands_to_list_test "hands_to_list has 0 hands for a 0 player game" 0;
    hands_to_list_test "hands_to_list has 1 hand for a 1 player game" 1;
    (* todo: test play_card once non-number card functionality is implemented
       play_card_test "play_card increment player index by 1" 1 4
       Game.get_curr_player (( + ) 0) (( + ) 0); play_card_test "play_card
       remove 1 card from player 0 hand" 6 4 Game.hands_to_list List.hd
       List.length; *)
    (* TODO: can also add more tests here for testing statuses - at start of
       game, all statuses are Normal *)
  ]

let tests =
  "unocaml test suite"
  >::: List.flatten [ card_tests; deck_tests; hand_tests; uno_tests ]

let _ = run_test_tt_main tests
