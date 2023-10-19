open Hand

(* error: This is an alias for module Unocaml__Hand, which is missing *)
open Deck
open Card

(* future issue to ponder: how to do reverse, skip, add card etc -> bc something
   to figure out is how to store other players' stuff *)
(* note: for now will be the same ish as game because it is one-player only *)

module type Move = sig end
(** A model that can progress the game based on a single move. *)
(* Each move is defined by card * hand * deck which is the card which was
   played, the new hand fo the players after that card was played, and*the new
   deck - if cards were drawn from it.) type t

   val update_game : t -> string -> t

   end*)

(** A model that keeps track of and updates the state of the game. *)
module type Game = sig
  type 'a t

  val build : unit -> 'a t
  (** Create a game of Uno. - makes default state - deals out 4 hands and picks
      random starter card and have leftover deck *)

  (* helpers: check if move is valid (takes in latest card & attempted move)
     shuffle deck: when deck runs out *)

  val play_card : 'a t -> 'b -> 'a t
  (** Progress the game based on a single move. *)

  val play_round : 'a t -> string -> 'a t
  (** Progress the game based on a user's move plus all the robot moves. *)
end

(* module Move : Move *)
module Game : Game

(** round type : ?? function to takes in user input and call other functions to
    play the move function called to check card - in game module : use as input
    the previous card played *)
