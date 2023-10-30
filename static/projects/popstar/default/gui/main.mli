open Js_of_ocaml

(** Initial GUI of a game.

    This module represents the initial setup of the game. The player would
    be able to select the setup. *)

val onload : 'a -> bool Js.t
(** [onload _ ] starts loading the game to a division attached 
    to the document. *)