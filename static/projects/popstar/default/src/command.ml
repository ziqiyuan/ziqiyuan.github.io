type coordinate = int * int

type command =
  | Coor of coordinate
  | Quit

exception Malformed

exception OutOfBound

let parse str size =
  match
    List.filter (fun x -> x <> "") (String.split_on_char ' ' str)
  with
  | [ h; m ] -> (
      try
        let x, y = (int_of_string h - 1, int_of_string m - 1) in
        if x >= 0 && y >= 0 && x <= size - 1 && y <= size - 1 then
          Coor (x, y)
        else raise OutOfBound
      with
      | Failure _ -> raise Malformed)
  | [ x ] -> if x = "quit" then Quit else raise Malformed
  | _ -> raise Malformed
