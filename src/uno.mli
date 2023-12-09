open Hand
open Card
open Deck

(** A model that keeps track of and updates the state of the game. *)
module type Game = sig
  type t
  (** Representation type of the model. *)

  (** Type representing the status of a player in the game. *)
  type status =
    | Normal
    | Uno
    | Won

  val get_deck : t -> Deck.t
  (** Return current deck. *)

  val get_curr_card : t -> Card.card
  (** Return current card. *)

  val get_curr_player : t -> int
  (** Return current player. *)

  val get_hand : t -> int -> Hand.t
  (** Return hand of a certain indexed player. *)

  val get_human_index : t -> int
  (** Return the index of the human player within the list of hands. Raises
      Invalid_argument if there is not at least one player in the game. *)

  val get_status : t -> int -> string
  (** Given a certain player's index, returns that player's status. Raises
      Invalid_argument if the player index is invalid. *)

  val get_curr_status : t -> string
  (** Given a current game state, returns the current player's status. Raises
      Invalid_argument if there is not at least one player in the game. *)

  val get_prev_status : t -> string
  (** Given a current game state, returns the previous player's status
      (determines previous player index using game's curr_player field). Raises
      Invalid_argument if there is not at least one player in the game.*)

  val hands_to_list : t -> Card.card list list
  (** Given a current game state, returns all the players' hands in the form of
      a list of card lists. *)

  val check_play : t -> Card.card -> bool
  (** Check if move is valid and return true if valid or false if invalid. *)

  val build : int -> t
  (** Create a game of Uno. - makes default state - deals out the hands and
      picks random starter card and have leftover deck. Takes in the number of
      players. *)

  val estimate_next_num_cards : t -> int -> int -> int
  (** Given a current game state and the index of the current player, returns an
      estimate (possibly correct) of the next player's number of cards. The
      second passed in int is representative of the difficulty inputted by users
      at the start - the more difficult, the smaller the number for a larger the
      estimate's range. (range is determined by 1/input) *)

  val play_card : t -> Card.card -> Hand.t -> t
  (** Progress the game based on a card to play. Takes in the hand where the
      card has been played from, which is equivalent to the current player's
      hand in the game minus the card to play. Ex: current player has a hand
      [{a, b, c}], card to play is [{b}], so input hand must be [{a, c}]. Raises
      Invalid_argument if the card to play is not valid by the rules of Uno. *)

  val robot_turn : t -> int -> t
  (** Given a current game state and the index of the current (robot) player,
      plays a robot's move. *)

  val handle_play : t -> bool -> string -> t
  (** [handle_play g b c] progresses the game based on a user's move. Raises
      Invalid_argument if [c] is [""], ["Any Wildcard"], or ["Any Wildcard4"]. *)
end

module Game : Game
(** A game of uno. *)
