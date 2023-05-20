open Core
open Scanf

let x, y = scanf "%d %d" Tuple2.create

let ans = if x < y then "Better" else "Worse"

let () = printf "%s\n%!" ans
