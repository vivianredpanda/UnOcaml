open Unocaml
open Hand
open Card
open Deck
open Uno

(* read-eval-print loop *)
(* let rec repl (eval : string -> string) : unit = print_string "> "; let input
   = read_line () in match input with | "" -> print_endline "bye" | _ -> input
   |> eval |> print_endline; repl eval *)

(* let split_space uu = String.split_on_char ' ' uu |> List.filter (fun elt ->
   elt <> "")

   let user_slipt uu = match split_space uu with | [] -> failwith "" | l -> l *)

(* let game_round game_state : Game.t = print_endline "Your cards : "; let
   curr_hand = Game.get_hand game_state (Game.get_curr_player game_state) in let
   game_state = Game.play_card game_state (Stdlib.fst (Deck.draw (Game.get_deck
   game_state))) curr_hand in if Game.get_curr_player game_state <> 0 then
   Game.robot_turn game_state (Game.get_curr_player game_state) else begin
   print_endline "Please enter your move: "; print_string "> "; let user_in =
   read_line () in let card_played = Card.to_card user_in in let game_state =
   Game.play_card game_state card_played curr_hand in (* print_endline "Your
   cards : "; *) game_state end *)

let rec pp_list acc lst =
  match lst with
  | [] -> acc
  | h :: t -> pp_list (h ^ ", " ^ acc) t

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to UnOCaml. \n";
  (*print_endline "\n\n\n";*)
  print_endline
    "\n\n\
    \ Here are the rules for the game: \n\
    \ Each player will be dealt 7 cards. The objective of the game is to play \n\n\
    \ all your cards the quickest. To play a card, it must match in either the \
     \n\n\
    \ number, color, or action of the previous card played. If no such card \n\n\
    \ exists, you must draw a card. If this card is valid, you may play it. \n\n\
    \ Play continues until one player has gotten rid of all their cards. \n\n\
    \ Please refer to the official Uno manuel for further clarifications. \n\n";
  print_endline "Please enter start:";
  print_string "> ";
  let start_line = read_line () in
  match start_line with
  | "start" ->
      print_endline "Dealing cards. . . ";
      let game_state = Game.build 1 in
      let game_round game_state : Game.t =
        print_endline "Your cards : ";
        let curr_hand =
          Game.get_hand game_state (Game.get_curr_player game_state)
        in
        print_endline
          (pp_list "" (Hand.to_list curr_hand |> List.map Card.string_of_card));
        let game_state =
          Game.play_card game_state
            (Stdlib.fst (Deck.draw (Game.get_deck game_state)))
            curr_hand
        in
        print_endline (Game.get_curr_card game_state |> Card.string_of_card);
        (* if Game.get_curr_player game_state <> 0 then Game.robot_turn
           game_state (Game.get_curr_player game_state) else begin *)
        print_endline "Please enter your move [color card]: ";
        print_string "> ";
        let user_in = read_line () in
        (* let card_played = Card.to_card user_in in *)
        print_endline (string_of_int (Game.get_curr_player game_state));
        let game_state =
          Game.handle_play game_state
            (Game.get_curr_player game_state = 0)
            ~card_input:user_in
        in
        game_state
        (* print_endline "Your cards : "; *)
        (* game_state end *)
      in
      if Game.get_curr_player game_state <> 0 then
        print_endline
          ("Player "
          ^ string_of_int (Game.get_curr_player game_state)
          ^ " played : "
          ^ Card.string_of_card (Game.get_curr_card game_state)
          ^ ".")
      else
        let game_state = game_round game_state in
        print_endline "Nice move! "
  | _ -> failwith "Expected 'start'."
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
