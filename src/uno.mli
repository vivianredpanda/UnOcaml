(* future issue to ponder: how to do reverse, skip, add card etc -> bc something
   to figure out is how to store other players' stuff *)
(* note: for now will be the same ish as game because it is one-player only *)

(* module type Move = sig end *)

(* Each move is defined by card * hand * deck which is the card which was
   played, the new hand fo the players after that card was played, and*the new
   deck - if cards were drawn from it.) type t

   val update_game : t -> string -> t

   end*)

open Hand
(** A model that can progress the game based on a single move. *)

open Card
open Deck

(** A model that keeps track of and updates the state of the game. *)
module type Game = sig
  type t

  val build : int -> t
  (** Create a game of Uno. - makes default state - deals out the hands and
      picks random starter card and have leftover deck. Takes in the number of
      players. *)

  (* helpers: check if move is valid (takes in latest card & attempted move)
     shuffle deck: when deck runs out *)

  val play_card : t -> Card.card -> Hand.t -> t
  (** Progress the game based on a card to play. Takes in the hand where the
      card has been played from, which is equivalent to the current player's 
      hand in the game minus the card to play. Ex: current player has a hand 
      {a, b, c}, card to play is {b}, so input hand must be {a, c}. Raises 
      Invalid_argument if the card to play is not valid by the rules of Uno. *)

  val handle_play : t -> bool -> ?card_input:string -> t
  (** Progress the game based on a user's move. *)
end

(* module Move : Move *)
module Game : Game

(** round type : ?? function to takes in user input and call other functions to
    play the move function called to check card - in game module : use as input
    the previous card played *)
