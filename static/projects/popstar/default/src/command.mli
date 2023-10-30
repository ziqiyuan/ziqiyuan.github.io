(** Parsing of player commands. *)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * It is part of the interface the course staff will use to test your
 * submission.
 **********************************************************************)

type coordinate = int * int

(** The type [coordinate] represents the valid coordinate of the block
    selected and entered by user.

    For example:

    - If the player command is ["3 5"], then the coordinate is (3,5).

    - If the player command is ["7     5"], then the object phrase is
      again (7,5). *)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command =
  | Coor of coordinate
  | Quit

exception Malformed
(** Raised when an illegal command is encountered. *)

exception OutOfBound
(** Raised when a coordinate is out of bound. *)

val parse : string -> int -> command
(** [parse str size] parses a player's input into a [command], as
    follows. The first number is the x-axis coordinate of the block; the
    second number is the y-axis coordinate of the block.

    - [parse "1 2"] is [Legal \(1, 2)\] - [parse "quit"] is [Quit].

    Requires: [str] contains only ints and space characters (only ASCII
    character code 32; not tabs or newlines, etc.).

    Raises: [OutOfBound] if the coordinate represented by [str] exceeds
    the [size] of the board.

    Raises: [Malformed] if the command is malformed. A command is {i
    malformed} if the verb is neither "quit" nor a representation of a
    coordinate.*)
