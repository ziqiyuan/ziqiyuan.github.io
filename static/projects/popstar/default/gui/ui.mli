open Js_of_ocaml
(** Main GUI of a game.

    This module represents the GUI where the player would be playing
    based on the selected game parameters. *)

val run : Dom_html.divElement Js.t -> int -> string -> string -> unit
(** [run div size theme mode] displays the board in the division [div] with a 
    board size of [size], game theme of [theme], and game mode of [mode]. *)

val remove_children : Dom_html.element Js.t -> unit
(** [remove_children n] remove all the children of node n. *)
