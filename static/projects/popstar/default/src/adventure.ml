open Array
open Random

(* Each int represents a color of the block. *)

type t = {
  levels : int;
  goals : int array;
}

(* maximum level, not the number of levels*)
let level = 9

let goal =
  let arr = make 10 0 in
  for x = 0 to length arr - 1 do
    if x mod 2 = 0 then arr.(x) <- (x * 1750) + 700
    else arr.(x) <- (x * 1750) + 350
  done;
  arr

let all_levels (round : t) : int = round.levels

let all_goals (round : t) : int array = round.goals

let init_game = { levels = level; goals = goal }
