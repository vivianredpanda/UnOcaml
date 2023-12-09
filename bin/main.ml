open Unocaml
open Hand
open Card
open Deck
open Uno

let rec pp_list acc lst =
  match lst with
  | [] -> acc
  | [ h ] -> h ^ acc
  | h :: t -> pp_list (", " ^ h ^ acc) t

let check_win s1 s2 = if s1 = "Won" || s2 = "Won" then true else false

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to UnOCaml. \n";
  print_endline
    "\n\n\
    \ Here are the rules for the game: \n\
    \ Each player will be dealt 7 cards. The objective of the game is to play \n\n\
    \ all your cards the quickest. To play a card, it must match in either the \
     \n\n\
    \ number, color, or action of the previous card played. If no such card \n\n\
    \ exists, you must draw a card. If this card is valid, you may play it. \n\n\
    \ Play continues until one player has gotten rid of all their cards. \n\n\
    \ Please refer to the official Uno manual for further clarifications. \n\n\
    \ Additionally, UnOCaml has a twist on traditional Uno. \n\n\
    \ Users can get an estimate of the next player's number of cards. \n\n\
    \ The accuracy of the estimate will be determined by the difficulty \n\n\
    \ that users input at the start of the game. The more difficult it is, \n\n\
    \ the more inaccurate the estimate is likely to be. \n\n";

  print_endline "If you want to use debug mode, please enter debug:";
  print_string "> ";
  let debug = String.lowercase_ascii (read_line ()) = "debug" in
  print_endline "Please enter the number of players:";
  print_string "> ";
  let num_players = read_line () in
  print_endline
    "Please enter the difficulty you would like to play at: easy, medium, or \
     hard.";
  print_endline
    "(Increasing difficulty means that estimates of the next player's number \
     of cards will be more inaccurate)";
  print_string "> ";
  let d = read_line () in
  let difficulty = if d = "easy" then 6 else if d = "medium" then 4 else 2 in

  match int_of_string num_players with
  | n ->
      (* Setup game and deal cards *)
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
        (* Prompt user for input *)
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
          print_endline
            "Please enter your move [color card] or 'Draw card' or 'Estimate \
             next':";
          print_string "> ";
          let user_in = read_line () in
          match String.lowercase_ascii user_in with
          (* Handles draw and either passing or playing the card. *)
          | "draw card" | "draw" | "d" ->
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
                        end)
              in
              handle_draw_input ()
          (* Handles user's prompt for estimating next players # of cards *)
          | "estimate next" | "estimate" | "e" ->
              print_endline
                ("Estimate that the next player has: "
                ^ string_of_int
                    (Game.estimate_next_num_cards !game_state curr_player
                       difficulty)
                ^ " cards. \n")
          (* Handles user input of a card to play. *)
          | _ -> (
              match Card.to_card user_in with
              | c -> (
                  let curr_player = Game.get_curr_player !game_state in
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
                      end)
              | exception Failure f ->
                  print_endline
                    "Incorrect input, please try again. Make sure cards are \
                     inputted like this: Color Card (eg: Red Wildcard, Blue \
                     Skip, Green 4, Yellow Plus 2)")
        end
        else
          (* If the user is a robot: *)
          let num_cards =
            List.length
              (List.nth
                 (Game.hands_to_list !game_state)
                 (Game.get_curr_player !game_state))
          in
          game_state :=
            Game.robot_turn !game_state (Game.get_curr_player !game_state);
          let n = List.length (Game.hands_to_list !game_state) in
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
