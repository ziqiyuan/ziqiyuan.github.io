open Adventure
open Command
open Array

(** [find_blocks_helper] updates the 2D array [arr] based on the index
    of the block [(x, y)] selected by the player. [value] is the value
    of the block selected. *)
let rec find_blocks_helper arr (x, y) value =
  arr.(x).(y) <- -1;
  if x = 0 && y = 0 then top_left arr (x, y) value
  else if x = 0 && y = length arr - 1 then top_right arr (x, y) value
  else if x = length arr - 1 && y = 0 then bottom_left arr (x, y) value
  else if x = length arr - 1 && y = length arr - 1 then
    bottom_right arr (x, y) value
  else if x = 0 then top_bar arr (x, y) value
  else if x = length arr - 1 then bottom_bar arr (x, y) value
  else if y = 0 then left_bar arr (x, y) value
  else if y = length arr - 1 then right_bar arr (x, y) value
  else center arr (x, y) value


and left arr (x, y) value =
  if arr.(x).(y - 1) = value then
    find_blocks_helper arr (x, y - 1) value
  else ()

and right arr (x, y) value =
  if arr.(x).(y + 1) = value then
    find_blocks_helper arr (x, y + 1) value
  else ()

and up arr (x, y) value =
  if arr.(x - 1).(y) = value then
    find_blocks_helper arr (x - 1, y) value
  else ()

and down arr (x, y) value =
  if arr.(x + 1).(y) = value then
    find_blocks_helper arr (x + 1, y) value
  else ()

and top_left arr (x, y) value =
  down arr (x, y) value;
  right arr (x, y) value

and top_right arr (x, y) value =
  left arr (x, y) value;
  down arr (x, y) value

and bottom_left arr (x, y) value =
  up arr (x, y) value;
  right arr (x, y) value

and bottom_right arr (x, y) value =
  up arr (x, y) value;
  left arr (x, y) value

and top_bar arr (x, y) value =
  left arr (x, y) value;
  right arr (x, y) value;
  down arr (x, y) value

and bottom_bar arr (x, y) value =
  left arr (x, y) value;
  right arr (x, y) value;
  up arr (x, y) value

and left_bar arr (x, y) value =
  up arr (x, y) value;
  down arr (x, y) value;
  right arr (x, y) value

and right_bar arr (x, y) value =
  up arr (x, y) value;
  down arr (x, y) value;
  left arr (x, y) value

and center arr (x, y) value =
  up arr (x, y) value;
  down arr (x, y) value;
  left arr (x, y) value;
  right arr (x, y) value

(** [count arr acc] is the number of -1 in the 1D array [arr]. *)
let rec count arr acc =
  match arr with
  | [||] -> acc
  | _ ->
      if arr.(0) = -1 then count (sub arr 1 (length arr - 1)) acc + 1
      else count (sub arr 1 (length arr - 1)) acc

let count_negative arr = fold_left (fun acc x -> count x acc) 0 arr

(** [popped_blocsks arr (x,y)] turns nearby blocks of index [(x,y)] in
    [arr]that have the same color/number as the chosen block into
    vancancy/-1. *)
let popped_blocks arr (x, y) =
  let tem = arr.(x).(y) in
  if tem = -1 then ()
  else
    let prev_count = count_negative arr in
    find_blocks_helper arr (x, y) tem;
    if count_negative arr - prev_count = 1 then arr.(x).(y) <- tem
    else ()

(** [swap arr (x,y)] swaps a block at [(x,y)] in array [arr] with a
    non(-1) block above it once if the block at the the coordinate (x,
    y) is (-1). *)
let swap arr (x, y) =
  try
    if arr.(x).(y) = -1 then
      for new_x = x - 1 downto 0 do
        if arr.(new_x).(y) <> -1 then (
          arr.(x).(y) <- arr.(new_x).(y);
          arr.(new_x).(y) <- -1;
          failwith "Already swapped once")
        else ()
      done
    else ()
  with
  | Failure _ -> ()

(** [move_down arr] moves all non(-1) blocks in array [arr] down below
    (-1) blocks. *)
let move_down arr =
  for x = length arr - 1 downto 0 do
    for y = length arr - 1 downto 0 do
      swap arr (x, y)
    done
  done

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

(** [filter_array arr] is true if all elements in the array [arr] is -1. *)
let filter_array arr = for_all (fun x -> x = -1) arr

(** [filter_matrix arr] filters out 1D array in 2D array [arr] whose
    elements are all -1. *)
let filter_matrix (arr : int array array) : int array list =
  List.filter (fun x -> not (filter_array x)) (to_list arr)

(** [fill_matrix arr] fills a matrix [arr] with -1 at the end to make it
    a square matrix. Requires: [arr] is not empty. *)
let fill_matrix arr =
  let height = length arr in
  let width = length arr.(0) in
  append arr (make_matrix (width - height) width (-1))

(** [move_left arr] moves empty colums in [arr] left. *)
let move_left arr =
  let filtered_list = arr |> transpose_sq |> filter_matrix in
  if filtered_list = [] then arr
  else filtered_list |> of_list |> fill_matrix |> transpose_sq

let update_blocks arr (x, y) =
  popped_blocks arr (x, y);
  move_down arr;
  move_left arr

let deep_copy arr : int array array =
  let size = Array.length arr in
  let copy = Array.make_matrix size size 0 in
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      copy.(x).(y) <- arr.(x).(y)
    done
  done;
  copy

(* get the number of neighbours out *)
let greedy_count arr (x, y) : int =
  let deep_copy_arr = deep_copy arr in
  let prev_num = count_negative arr in
  let value = arr.(x).(y) in
  if value <> -1 then (
    find_blocks_helper deep_copy_arr (x, y) value;
    let num_neg = count_negative deep_copy_arr in
    let difference = num_neg - prev_num in
    if difference = 1 then 0 else difference)
  else 0

(* type list = ((x *y) * neg) list *)
type coor = int * int

let find_max (lst : (int * coor) list) =
  let neg, coor = List.split lst in
  let max = List.sort Stdlib.compare neg |> List.rev |> List.hd in
  List.assoc max lst

let ai arr =
  let assoc = ref [] in
  for x = 0 to length arr - 1 do
    for y = 0 to length arr - 1 do
      assoc := (greedy_count arr (x, y), (x, y)) :: !assoc
    done
  done;
  find_max !assoc
