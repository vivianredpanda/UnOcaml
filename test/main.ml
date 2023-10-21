open OUnit2
open Unocaml
open Deck
open Card

let pp_pair pp_l pp_r (l, r) = Printf.sprintf "(%s, %s)" (pp_l l) (pp_r r)

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

let deck2 = Deck.reset ()

let deal_test out in1 _ =
  assert_equal ~printer:string_of_int
    ~msg:
      ("function: deal\ninputs: %s %s"
      ^ pp_list Card.string_of_card (Deck.to_list in1))
    out
    (Deck.deal in1 |> fst |> Deck.to_list |> List.length)

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
  ]

let hand_tests = []
let uno_tests = []

let tests =
  "unocaml test suite"
  >::: List.flatten [ card_tests; deck_tests; hand_tests; uno_tests ]

let _ = run_test_tt_main tests
