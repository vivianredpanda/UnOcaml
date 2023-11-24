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
  print_endline "Please enter the number of players:";
  print_string "> ";
  let num_players = read_line () in
  match num_players with
  | "1" ->
      print_endline "Dealing cards. . . ";
      let game_state = ref (Game.build 1) in
      let curr_hand =
        Game.get_hand !game_state (Game.get_curr_player !game_state)
      in
      game_state :=
        Game.play_card !game_state
          (Stdlib.fst (Deck.draw (Game.get_deck !game_state)))
          curr_hand;
      while
        not (List.length (Hand.to_list (Game.get_hand !game_state 0)) = 0)
      do
        print_endline "Your cards : ";
        let curr_hand =
          Game.get_hand !game_state (Game.get_curr_player !game_state)
        in
        print_endline
          (pp_list "" (Hand.to_list curr_hand |> List.map Card.string_of_card));
        print_endline
          ("Current Card : "
          ^ Card.string_of_card (Game.get_curr_card !game_state));
        print_endline "Please enter your move [color card] or 'Draw card': ";
        print_string "> ";
        let user_in = read_line () in
        match user_in with
        | "Draw card" ->
            game_state :=
              Game.handle_play !game_state
                (Game.get_curr_player !game_state = 0)
                user_in;
            print_endline "Your new cards : ";
            let n_curr_hand =
              Game.get_hand !game_state (Game.get_curr_player !game_state)
            in
            print_endline
              (pp_list ""
                 (Hand.to_list n_curr_hand |> List.map Card.string_of_card));
            print_endline
              "Do you want to play this card? ('Pass' or enter card to play)";
            print_string "> ";
            let d_user_in = read_line () in
            game_state :=
              Game.handle_play !game_state
                (Game.get_curr_player !game_state = 0)
                d_user_in;
            print_endline "Nice move!"
        | "Pass" -> failwith "Invalid Move."
        | _ -> (
            (* print_endline (string_of_int (Game.get_curr_player
               !game_state)); *)
            game_state :=
              Game.handle_play !game_state
                (Game.get_curr_player !game_state = 0)
                user_in;
            print_endline "Nice move!";
            print_endline "Anything to say?";
            print_string "> ";
            let if_uno = read_line () in
            match if_uno with
            | "Uno" -> begin
                if List.length (Hand.to_list (Game.get_hand !game_state 0)) = 1
                then print_endline "UnOCaml"
                else print_endline "Incorrect UnOCaml";
                game_state :=
                  Game.handle_play !game_state
                    (Game.get_curr_player !game_state = 0)
                    "Draw Card"
              end
            | _ -> begin
                if List.length (Hand.to_list (Game.get_hand !game_state 0)) = 1
                then (
                  print_endline "Missed UnOCaml";
                  game_state :=
                    Game.handle_play !game_state
                      (Game.get_curr_player !game_state = 0)
                      "Draw card")
                else print_endline "No UnOCaml"
              end)
      done
      (* if (List.length (Hand.to_list (Game.get_hand !game_state 0)) = 0)
         then *)
  | _ -> failwith "Failure : Multi-player Game Unimplemented."
