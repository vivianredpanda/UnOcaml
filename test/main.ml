open OUnit2
open Unocaml
open Deck
open Card

let card_tests = []
let deck_tests = []
let hand_tests = []
let uno_tests = []

let tests =
  "unocaml test suite"
  >::: List.flatten [ card_tests; deck_tests; hand_tests; uno_tests ]

let _ = run_test_tt_main tests
