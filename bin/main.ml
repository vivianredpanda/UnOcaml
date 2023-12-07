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
  | [ h ] -> h ^ acc
  | h :: t -> pp_list (", " ^ h ^ acc) t

let check_win s1 s2 = if s1 = "Won" || s2 = "Won" then true else false

(* let draw (game_s : Game.t) = let card_drawn = Deck.draw (Game.get_deck
   game_s) in let new_hand = Hand.add_card (Stdlib.fst card_drawn)
   (Game.get_hand game_s (Game.get_curr_player game_s)) in { curr_deck :
   Stdlib.snd card_drawn; curr_card : Card.card; curr_player : int; hands :
   Hand.t list; human_index : int; } game_s *)

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
  (* TODO: add instructions for how to input cards *)
  print_endline "If you want to use debug mode, please enter debug:";
  print_string "> ";
  let debug = String.lowercase_ascii (read_line ()) = "debug" in
  print_endline "Please enter the number of players:";
  print_string "> ";
  let num_players = read_line () in
  (* while (int_of_string num_players <= 4 && int_of_string num_players >= 1) *)
  match int_of_string num_players with
  | n ->
      print_endline "Dealing cards. . . ";
      let game_state = ref (Game.build n) in
      let curr_hand =
        Game.get_hand !game_state (Game.get_curr_player !game_state)
      in
      game_state :=
        Game.play_card !game_state
          (Stdlib.fst (Deck.draw (Game.get_deck !game_state)))
          curr_hand;
      while
        not
          (check_win
             (Game.get_curr_status !game_state)
             (Game.get_prev_status !game_state))
      do
        let curr_player = Game.get_curr_player !game_state in
        if curr_player = Game.get_human_index !game_state then begin
          print_endline "Your cards : ";
          let curr_hand = Game.get_hand !game_state curr_player in
          print_endline
            (pp_list ""
               (Hand.to_list curr_hand |> List.map Card.string_of_card));
          print_endline
            ("Current Card : "
            ^ Card.string_of_card (Game.get_curr_card !game_state));
          print_endline "Please enter your move [color card] or 'Draw card': ";
          print_string "> ";
          let user_in = read_line () in
          match String.lowercase_ascii user_in with
          | "draw card" | "draw" ->
              let new_game =
                Game.handle_play !game_state
                  (curr_player = Game.get_human_index !game_state)
                  "Draw card"
              in
              game_state := new_game;
              print_endline "Your new cards : ";
              let n_curr_hand = Game.get_hand !game_state curr_player in
              print_endline
                (pp_list ""
                   (Hand.to_list n_curr_hand |> List.map Card.string_of_card));
              let rec handle_draw_input () =
                print_endline
                  "Do you want to play this card? ('Pass' or enter card to \
                   play)";
                print_string "> ";
                let d_user_in = read_line () in
                match String.lowercase_ascii d_user_in with
                | "pass" ->
                    game_state :=
                      Game.handle_play !game_state
                        (curr_player = Game.get_human_index !game_state)
                        "Pass"
                | _ -> (
                    match
                      Game.handle_play !game_state
                        (curr_player = Game.get_human_index !game_state)
                        d_user_in
                    with
                    | (exception Invalid_argument i) | (exception Failure i) ->
                        print_endline "Please double check your input";
                        handle_draw_input ()
                    | _ ->
                        game_state :=
                          Game.handle_play !game_state
                            (curr_player = Game.get_human_index !game_state)
                            d_user_in;
                        print_endline "Nice move!")
              in
              handle_draw_input ()
          | _ -> (
              match Card.to_card user_in with
              | c -> (
                  let curr_player = Game.get_curr_player !game_state in
                  (* let new_game = Game.handle_play !game_state (curr_player =
                     Game.get_human_index !game_state) user_in *)
                  (* in *)
                  match
                    Game.handle_play !game_state
                      (curr_player = Game.get_human_index !game_state)
                      user_in
                  with
                  | exception Invalid_argument i ->
                      print_endline "Invalid move please try again";
                      print_endline ("Error of: " ^ i)
                  | _ ->
                      game_state :=
                        Game.handle_play !game_state
                          (curr_player = Game.get_human_index !game_state)
                          user_in;
                      print_endline "Nice move!";
                      if Game.get_prev_status !game_state = "Uno" then begin
                        print_endline "Anything to say?";
                        print_string "> ";
                        let if_uno = read_line () in
                        match String.lowercase_ascii if_uno with
                        | "uno" -> print_endline "UnOCaml"
                        | _ ->
                            print_endline "Missed UnOCaml. Drawing card.";
                            game_state :=
                              Game.handle_play !game_state true "missed uno"
                        (*TO_DO : add in uno.ml handel_play this option and itll
                          draw card for prev player & update status back to
                          Normal*)
                      end)
              | exception Failure f ->
                  print_endline
                    "Incorrect input, please try again. Make sure cards are \
                     inputted like this: Color Card (eg: Red Wildcard, Blue \
                     Skip, Green 4, Yellow Plus 2)")
        end
        else
          let num_cards =
            List.length
              (List.nth
                 (Game.hands_to_list !game_state)
                 (Game.get_curr_player !game_state))
          in
          game_state :=
            Game.robot_turn !game_state (Game.get_curr_player !game_state);
          let n = List.length (Game.hands_to_list !game_state) in
          (* TODO: the indices aren't really consistent especially after playing
             Skip or Reverse cards *)
          let prev_index =
            match Game.get_curr_card !game_state with
            | Skip _ -> (Game.get_curr_player !game_state - 2 + n) mod n
            | _ -> (Game.get_curr_player !game_state - 1 + n) mod n
          in
          let new_num_cards =
            List.length (List.nth (Game.hands_to_list !game_state) prev_index)
          in
          if new_num_cards > num_cards then
            print_endline
              ("Robot " ^ string_of_int prev_index ^ " drew a card and passed")
          else if new_num_cards = num_cards then
            print_endline
              ("Robot " ^ string_of_int prev_index
             ^ " drew a card then played : "
              ^ Card.string_of_card (Game.get_curr_card !game_state))
          else
            print_endline
              ("Robot " ^ string_of_int prev_index ^ " played : "
              ^ Card.string_of_card (Game.get_curr_card !game_state));

          if Game.get_status !game_state prev_index = "Uno" then
            print_endline ("Robot " ^ string_of_int prev_index ^ " is at Uno")
          else if Game.get_status !game_state prev_index = "Won" then
            print_endline ("Robot " ^ string_of_int prev_index ^ " won")
          else ();
          if debug then
            let hs = Game.hands_to_list !game_state in
            let string_of_hand h =
              pp_list "" (List.map Card.string_of_card h)
            in
            print_endline
              ("\n" ^ String.concat "\n" (List.map string_of_hand hs) ^ "\n")
          else ()
      done
(* if (Game.get_prev_status !game_state = "Won") then print_endline "YOU WON" in
   () else print_endline "" in (); *)
