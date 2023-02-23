open Core
open Scanf

let r, c = scanf "%d %d" Tuple2.create

let ans = if max (abs @@ r - 8) (abs @@ c - 8) % 2 = 1 then "black" else "white"

let () = printf "%s\n%!" ans
