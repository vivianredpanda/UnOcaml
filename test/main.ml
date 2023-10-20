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
    >:: get_color_test (Card.to_color "Any") (Card.to_card "Any Wildcard");
    "get_color of Used Wildcard"
    >:: get_color_test (Card.to_color "Blue") (Card.to_card "Blue Wildcard");
    "get_color of Unused Wildcard4"
    >:: get_color_test (Card.to_color "Any") (Card.to_card "Any Wildcard4");
    "get_color of Used Wildcard4"
    >:: get_color_test (Card.to_color "Blue") (Card.to_card "Blue Wildcard4");
    "get_color of Number Card"
    >:: get_color_test (Card.to_color "Green") (Card.to_card "Green 5");
    "get_color of Reverse"
    >:: get_color_test (Card.to_color "Yellow") (Card.to_card "Yellow Reverse");
    "get_color of Skip"
    >:: get_color_test (Card.to_color "Red") (Card.to_card "Red Skip");
    "get_color of Plus Two"
    >:: get_color_test (Card.to_color "Blue") (Card.to_card "Blue Plus 2");
    "get_color of Plus Four"
    >:: get_color_test (Card.to_color "Red") (Card.to_card "Red Plus 4");
    "get_number of Unused Wildcard"
    >:: get_number_test (-1) (Card.to_card "Any Wildcard");
    "get_number of Used Wildcard"
    >:: get_number_test (-1) (Card.to_card "Blue Wildcard");
    "get_number of Unused Wildcard4"
    >:: get_number_test (-1) (Card.to_card "Any Wildcard4");
    "get_number of Used Wildcard4"
    >:: get_number_test (-1) (Card.to_card "Blue Wildcard4");
    "get_number of Number Card" >:: get_number_test 5 (Card.to_card "Green 5");
    "get_number of Reverse"
    >:: get_number_test (-1) (Card.to_card "Yellow Reverse");
    "get_number of Skip" >:: get_number_test (-1) (Card.to_card "Red Skip");
    "get_number of Plus Two" >:: get_number_test 2 (Card.to_card "Blue Plus 2");
    "get_number of Plus Four" >:: get_number_test 4 (Card.to_card "Red Plus 4");
  ]

let deck_tests = []
let hand_tests = []
let uno_tests = []

let tests =
  "unocaml test suite"
  >::: List.flatten [ card_tests; deck_tests; hand_tests; uno_tests ]

let _ = run_test_tt_main tests
