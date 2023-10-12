open OUnit2

let tests = "unocaml test suite" >::: []
let _ = run_test_tt_main tests
