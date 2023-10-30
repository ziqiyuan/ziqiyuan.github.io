open Game
open Adventure
open Command
open Change
open Array
open State

(** [chosen_index str] is the index chosen by the string [str] entered
    by the player. *)
let chosen_index str size =
  match parse str size with
  | Coor (x, y) -> (x, y)
  | Quit ->
      print_endline "Bye!";
      exit 0

(* [adjusted_goal game state] is the goal of the current [game] and
   [state] adjusted to be size*size/100 of a game of board size 10. *)
let adjusted_goal game state =
  let n = current_size state in
  (all_goals game).(current_level state) * n * n / 100

(** [print_goal] prints the goal of the current level. *)
let print_goal game state =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("Goal: " ^ string_of_int (adjusted_goal game state) ^ "  ")

(** [print_header] prints the goal, score, and level of the current
    round. *)
let print_header game state =
  print_goal game state;
  print_round state

(** [play_game game state] plays the game based on the current game
    setup and state of the current round. *)
let rec play_game (game : Adventure.t) (state : State.t) : unit =
  if current_level state > all_levels game then
    print_endline "Congratulations. You win!"
  else if not (live_board (current_board state)) then
    if current_score state < adjusted_goal game state then (
      print_header game state;
      print_endline "Sorry. You lose!")
    else play_game game (update_level state)
  else (
    print_header game state;
    let str = read_line () in
    match chosen_index str (current_size state) with
    | x, y -> play_game game (update_round state (x, y))
    | (exception OutOfBound)
    | (exception Malformed) ->
        print_endline "*****Invalid input. Please enter again.*****";
        play_game game state)

(** [print_instructions] prints the instructions for the game. *)
let print_instructions =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "Welcome to the popstar game!\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "How to play:\n\
     Type in an integer n to set up the board size (n * n) you want. \n\
     Type in the coordinate with two or more adjacent blocks sharing \
     the same symbol to eliminate. \n\
     No time limit, but each stage has target points to enter the next \
     level. \n\
     Type in “quit” to quit the game  \n\n\
     Tips on Scoring:\n\
     Remember just two rules below:\n\
     The more blocks you pop in a single move, the more score you will \
     get.\n\
     Try to clear all blocks, you will get a lot of bonus. \n";
  print_endline "Please enter the size of the board (between 2 and 50)."

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_instructions;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | n -> (
      try
        let size = int_of_string n in
        if size < 2 || size > 50 then failwith "Invalid Input"
        else
          let start = initiate (int_of_string n) in
          play_game init_game start
      with
      | Failure _ -> print_endline "Invalid input.")

(* Execute the game engine. *)
let () = main ()
