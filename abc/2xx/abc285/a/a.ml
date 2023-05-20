open Core

let a, b = Scanf.scanf "%d %d" Tuple2.create
let a = min a b and b = max a b

let yes =
  match a with
  | 1 -> b = 2 || b = 3
  | 2 -> b = 4 || b = 5
  | 3 -> b = 6 || b = 7
  | 4 -> b = 8 || b = 9
  | 5 -> b = 10 || b = 11
  | 6 -> b = 12 || b = 13
  | 7 -> b = 14 || b = 15
  | _ -> false

let () = printf "%s\n%!" @@ if yes then "Yes" else "No"
