open Js_of_ocaml
module Html = Dom_html
open Game
open Adventure
open State

(** [js s] converts string s to javastring. *)
let js = Js.string

(** [document] is a webpage opened in a  website window. *)
let document = Html.window##.document

(** [timer] is the time the player has been playing. *)
let timer = ref 0

(** [draw_timer ()] is the timer as a paragraph element. *)
let draw_timer () = 
  let p = Html.createP document in 
  p##.id := js "timer";
  p##.innerHTML := js ("Time passed: " ^ string_of_int !timer);
  p##.style##.color := js "red";
  p##.style##.fontSize := js "25px";
  p

(** [get_element name] gets the element that has the id [name]. *)
let get_element name=
  Js.Opt.get (document##getElementById (js name))
  (fun () -> assert false)

(** [remove_image n] remove all the images of node n. *)
let remove_image n = 
  let lst = n##.childNodes in 
  for i = lst##.length - 1 downto 0 do
    Js.Opt.iter (lst##item i) (fun c -> 
    if Js.to_string c##.nodeName = "IMG" then
    (Dom.removeChild n c)
    )
  done

let remove_children n =
  while n##hasChildNodes |> Js.to_bool do
    Dom.removeChild n
    (Js.Opt.get n##.firstChild (fun _ -> raise Not_found))
  done

(** [block_image] is the image for the board selected based on 
    the value at the block [block_val]. *)
let image_factory name block_val =
  js
      ( if block_val = -1 then "stars/empty.png" else
        if block_val = 0 then "stars/" ^ name ^ "/5.png" else
        if block_val = 1 then "stars/" ^ name ^ "/1.png" else
        if block_val = 2 then "stars/" ^ name ^ "/2.png" else
        if block_val = 3 then "stars/" ^ name ^ "/3.png" else
        if block_val = 4 then "stars/" ^ name ^ "/4.png" else
          "stars/empty.png"
      )

(** [adjusted_goal game state] is the goal of the current [game] and
   [state] adjusted to be size*size/100 of a game of board size 10. *)
let adjusted_goal game state =
  let n = current_size state in
  (all_goals game).(current_level state) * n * n / 100

(** [sound_effect name] is an autoplay audio element with the audio from 
file [name]. *)
let sound_effect name= 
  let sound = Html.createAudio document in
  sound##.src := js ("sound_effect/" ^ name ^ ".mp3");
  sound##.autoplay := Js._true;
  sound##.loop := Js._false;
  sound

(** [win_game d] attaches a fragment for winning the game to division [d]. *)
let win_game d = 
  remove_children d;
  let frag = document##createDocumentFragment in
  let p = Html.createP document in 
  p##.innerHTML := js "Congratulations. You win!";
  p##.style##.color := js "red";
  p##.style##.fontSize := js "50px";
  let img = Html.createImg document in
  img##.src := js "stars/camelflage.png";
  Dom.appendChild frag p;
  Dom.appendChild frag img;
  Dom.appendChild frag (sound_effect "win");
  Dom.appendChild d frag
  
(** [lose_hint hint] is the paragraph element for the reason [hint] of 
    losing the game. *)
let lose_hint hint = 
  let p = Html.createP document in 
  p##.innerHTML := js hint;
  p##.style##.color := js "purple";
  p##.style##.fontSize := js "40px";
  p

(** [lose_game d] attaches a fragment for losing the game to division [d]. *)
let lose_game d = 
  remove_children d;
  let frag = document##createDocumentFragment in
  let p = Html.createP document in 
  p##.innerHTML := js "Oh no. You lose!";
  p##.style##.color := js "grey";
  p##.style##.fontSize := js "50px";
  let img = Html.createImg document in
  img##.src := js "stars/sadcamel.png";
  Dom.appendChild frag p;
  Dom.appendChild frag img;
  Dom.appendChild frag (lose_hint "You failed to reach the target score.");
  Dom.appendChild frag (sound_effect "lose");
  Dom.appendChild d frag

(** [lose_game_countdown d] displayes the page for losing a game in the 
    countdown mode. *)
let lose_game_countdown d = 
  lose_game d;
  Dom.appendChild d (lose_hint "Time is up!")

(** [update_timer d mode] updates the timer in division [d] as time goes by. 
    The timer would stop updating and the player would lose the game if the 
    [mode] is countdown mode and 60s has passed. *)
let update_timer d mode =
  timer := !timer + 1;
  if mode = "Countdown" && !timer > 60 then lose_game_countdown d else ();
  Dom.replaceChild d (draw_timer ()) (get_element "timer")

(** [score_board game state] is the score board as a paragraph element
 based on the current [game] and [state]. *)
let score_board game state =   
  let p = Html.createP document in 
  p##.id := js "score";
  p##.innerHTML := js ("Goal: " ^ string_of_int (adjusted_goal game state) 
  ^ " Score: "^ string_of_int (current_score state) ^ " Level: " ^ 
  string_of_int (current_level state));
  p##.style##.color := js "#795548";
  p##.style##.fontSize := js "40px";
  p

(** [init_board game state div theme] attaches a fragment containing the 
   board of the current state [st] in the game [game] with theme [theme]
   to the division [div]. *)
let rec init_board game state div theme =
  if current_level state > all_levels game then (win_game div)
  else if not (live_board (current_board state)) && 
    current_score state < adjusted_goal game state then (lose_game div)
  else ();
  remove_image div;
  let frag = draw_board game state div theme in
  div##.style##.lineHeight := js "0";
  let p = score_board game state in
  Dom.replaceChild div p (get_element "score");
  Dom.appendChild div frag

(** [draw_board game state div theme] draws the board of the current state 
[st] in the game [game] with theme [theme] to the division [div]. *)
and draw_board game state div theme =
  let bd = current_board state in 
  let size = Array.length bd in
  let frag = document##createDocumentFragment in
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      let img = Html.createImg document in
      img##.src := image_factory theme bd.(x).(y); 
      img##.width := 50;
      img##.height := 50;
      if bd.(x).(y) <> -1 then img##.onclick := Html.handler (fun _ ->
      pop_blocks game state div (x,y) theme;
      Js._false;)
      else ();
      Dom.appendChild frag img
    done;
    Dom.appendChild frag (Html.createBr document)
  done;
  frag

(** [pop_blocks game state div (x,y) theme] determines the next board to 
  be displayed based on the selection of block at [(x,y)]. The game board is 
  to be displayed on division [div] in the current [game] and [state]
  with the theme [theme]. *)
and pop_blocks game state div (x,y) theme = 
  Dom.appendChild div (sound_effect "pop");
  let state = update_round state (x, y) in
  if (current_level state) + 1 > all_levels game then win_game div
  else if not (live_board (current_board state)) then
    if current_score state < adjusted_goal game state then lose_game div
    else init_board game (update_level state) div theme
  else init_board game state div theme

let run div size theme mode =
  Dom.appendChild div (draw_timer ());
  Html.window##setInterval (* Update timer every 1s. *)
  (Js.wrap_callback (fun _ -> 
    update_timer div mode)) 1000. |> ignore;
  let p = Html.createP document in 
  p##.id := js "score"; 
  Dom.appendChild div p;
  Dom.appendChild div (
    let sound = sound_effect "game" in 
    sound##.loop := Js._true;
    sound
  );
  init_board init_game (initiate size) div theme
