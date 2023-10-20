open Unocaml

(* read-eval-print loop *)
(* let rec repl (eval : string -> string) : unit = print_string "> "; let input
   = read_line () in match input with | "" -> print_endline "bye" | _ -> input
   |> eval |> print_endline; repl eval *)

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to UnOCaml. \n";
  print_endline "\n\n\n";
  print_endline
    "\n\n\
    \ Here are the rules for the game. \n\
    \ Each player will be dealt 7 cards.\n\n\
    \  The objective of the game is to be the first\n";
  print_endline "Please enter the training file (corpus):";
  print_string "> "
(* let file = read_line () in print_endline "Reading file..."; let input = file
   |> In_channel.open_text |> In_channel.input_all in print_endline "Please
   enter the type of model you want to build: \n\ \"random N\", \"best N\", or
   \"interp N\", where N is the n-gram size."; print_string "> "; match
   String.split_on_char ' ' (read_line ()) with | [ "random"; n ] ->
   print_endline "Building random model (this may take a while)..."; let ngram =
   build_rand_ngram input (int_of_string n) in print_endline "Done building
   random model!"; print_string "Please enter the max number of words to
   generate: "; let max_len = int_of_string (read_line ()) in print_endline
   "Ready to chat! (Ctrl-D or blank to quit.)"; repl (create_rand_sequence ngram
   max_len) | [ "best"; n ] -> print_endline "Building most-frequent model (this
   may take a while)..."; let ngram = build_freq_ngram input (int_of_string n)
   in print_endline "Done building most-frequent model!"; print_string "Please
   enter the max number of words to generate: "; let max_len = int_of_string
   (read_line ()) in print_endline "Ready to chat! (Ctrl-D or blank to quit.)";
   repl (create_freq_sequence ngram max_len) | [ "interp"; n ] -> print_endline
   "Building interpolated model (this may take a while)..."; let ngram =
   build_interp_ngram input (int_of_string n) in print_endline "Done building
   interpolated model!"; print_string "Please enter the max number of words to
   generate: "; let max_len = int_of_string (read_line ()) in print_endline
   "Ready to chat! (Ctrl-D or blank to quit.)"; repl (create_interp_sequence
   ngram max_len) | _ -> failwith "Expected exactly two arguments." *)
