open Array
open Random
open Printf
open Change

(* Each int represents a color of the block. *)
type board = int array array

type level = int

type score = int

type size = int

type t = {
  board : board;
  level : level;
  score : score;
  size : size;
  aiboard : board;
}

let current_board t = t.board

let current_level t = t.level

let current_score t = t.score

let current_size t = t.size

(** [boardtwice f arr] applies function f to each element of array [arr]
    twice. *)
let maptwice f arr = map (fun arr1 -> map f arr1) arr

(** [transpose_sq arr] is the transpose of an integer square 2D array
    [arr] *)
let transpose_sq arr =
  let result = make_matrix (length arr) (length arr) 0 in
  for y = 0 to length arr - 1 do
    for x = 0 to length arr - 1 do
      result.(y).(x) <- arr.(x).(y)
    done
  done;
  result

(* row_check using list *)
(* let rec row_check arr = let lst = to_list arr in match lst with | h
   :: [] -> false | h :: t -> if h = List.hd t then true else row_check
   (of_list t) | _ -> false *)

(** [row_check arr] returns true if a 1D array [arr] has at least two
    equal elements next to each other. *)
let rec row_check arr =
  match arr with
  | [||] -> false
  | [| _ |] -> false
  | _ ->
      (arr.(0) = arr.(1) && arr.(0) <> -1)
      || row_check (sub arr 1 (length arr - 1))

(** [check arr] applies [row_check] to each row of a 2D array [arr]. *)
let check arr = map row_check arr

let live_board arrs =
  Array.fold_right (fun x acc -> x || acc) (check arrs) false
  || Array.fold_right
       (fun x acc -> x || acc)
       (check (transpose_sq arrs))
       false

(** [product_board n] is an [n] by [n] square matrix randomly filled
    with 0, 1, 2, 3, 4. *)
let rec product_board n =
  Random.self_init ();
  let board =
    maptwice (fun _ -> Random.int 5) (Array.make_matrix n n 0)
  in
  if live_board board then board else product_board n

(* let rec transpose list = match list with | [] -> [] | [] :: t ->
   transpose t | (h::t) :: rest_rows -> (h :: List.board List.hd
   rest_rows) :: transpose (t :: List.map List.tl rest_rows) *)

(** [print_board_row arr] prints a row of the array [arr] *)
let print_board_row arr =
  iter
    (fun x ->
      if x <> -1 then print_string (string_of_int x ^ " ")
      else print_string "  ")
    arr

let human_machine state : int =
  let human = count_negative state.board in
  let machine = count_negative state.aiboard in
  human - machine

let print_competition state =
  let diff = human_machine state in
  if diff > 0 then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "You are leading the game.";
    print_endline "")
  else if diff < 0 then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "AI is taking the lead. ";
    print_endline "")
  else (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "It's a tie right now.";
    print_endline "")

let print_round st =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("Score: " ^ string_of_int st.score ^ "  Level: "
   ^ string_of_int st.level ^ "\n");
  iter
    (fun x ->
      print_board_row x;
      print_endline "")
    st.board;
  print_endline "";
  print_endline "AI steps: ";
  iter
    (fun x ->
      print_board_row x;
      print_endline "")
    st.aiboard;
  print_endline "";
  print_competition st

let live_board arrs =
  Array.fold_right (fun x acc -> x || acc) (check arrs) false
  || Array.fold_right
       (fun x acc -> x || acc)
       (check (transpose_sq arrs))
       false

let rec initiate size =
  let b = product_board size in
  if live_board b then
    {
      board = b;
      level = 0;
      score = 0;
      size;
      aiboard = Change.deep_copy b;
    }
  else initiate size

(** [bonus count size] is the bonus score based on the [count] of newly
    popped blocks and the [size] of the current board. *)
let bonus count size =
  if count <= size then 2000 - (2 * size * count * count) else 0

(* changed *)
let update_round prev ((x, y) : int * int) =
  let prev_count = count_negative prev.board in
  let new_board = update_blocks prev.board (x, y) in
  {
    (* prev with *)
    board = new_board;
    level = prev.level;
    score =
      (let n = count_negative new_board in
       let diff = n - prev_count in
       let s = prev.size in
       prev.score
       + (diff * diff * 5)
       + (bonus ((s * s) - n) prev.size * s * s / 100));
    size = prev.size;
    aiboard = update_blocks prev.aiboard (Change.ai prev.aiboard);
  }

let update_level (prev : t) : t =
  let new_board = product_board prev.size in
  {
    board = new_board;
    level = prev.level + 1;
    score = prev.score;
    size = prev.size;
    aiboard = deep_copy new_board;
  }
