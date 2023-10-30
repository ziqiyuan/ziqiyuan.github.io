(** Representation of a game.

    This module represents the information of the current game,
    including levels and scoring goals.*)

type t
(** The abstract type of values representing a game. *)

val all_levels : t -> int
(** [all_levels round] is the list of levels in a game. *)

val all_goals : t -> int array
(** [all_goals round] is the list of target scores the player has to
    pass in each level. *)

val init_game : t
(** [init_game] is the initial setup of the game. *)
