(** Representation of a state of a round.

    This module represents the information of the current round,
    including the board, the level, and player's score. It also updates
    the state based on chosen coordinates. *)

type t
(** The abstract type of values representing a round. *)

type board = int array array
(** The board visualization of a round. *)

type level = int
(** The level the round is on. *)

type score = int
(** The player's score. *)

val current_board : t -> int array array
(** [current_board is the current board. *)

val current_level : t -> int
(** [current_level] is the level of the current round. *)

val current_score : t -> int
(** [current_score] is the current score. *)

val current_size : t -> int
(** [current_size] is the length of the current board, that is, the
    board has a dimension of size*size. *)

val live_board : board -> bool
(** [live_board m] is a boolean that returns whether a board is eligible
    to be played. *)

val print_round : t -> unit
(** [print_board st] prints the visualization of current round [t]. *)

val initiate : int -> t
(** [initiate n] initiates the state with a board of size [n] when the
    game starts. *)

val update_round : t -> int * int -> t
(** [update_round old_rd index new_rd] is the new round state [new_rd]
    after the user types in a command to pop off the block at [index] in
    previous round [old_rd]. The new score is based on the count of
    blocks popped off. *)

val update_level : t -> t
(** [update_level old_rd new_rd] is the new round [new_rd] at a higher
    level after passing the current game [old_rd]. A new board is
    generated. *)
