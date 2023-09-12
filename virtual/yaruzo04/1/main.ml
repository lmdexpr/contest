open Core
open Scanf

let x, a, b = scanf "%d %d %d" Tuple3.create

let ans = if abs (x - a) < abs (x - b) then "A" else "B"

let () = printf "%s\n%!" ans
