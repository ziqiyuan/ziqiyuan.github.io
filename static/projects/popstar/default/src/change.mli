(** Change for the game.

    This module represents the updates when the player chooses a block
    and advances to the next round. *)

val update_blocks : int array array -> int * int -> int array array
(** [update_blocks board cd] is the new board based on the coordinate
    [cd] chosen in the current board [board]. *)

val count_negative : int array array -> int
(** [count_negative arr] is the number of -1 in the 2D array [arr]. *)

val ai : int array array -> int * int
(** [ai arr] returns the coordinate the ai will pop off the board [arr].
    **)

val deep_copy : int array array -> int array array
(**[deep_copy arr] returns an board identical to [arr] but with a different
   pointer**)
