open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let ans = if a * b % 2 = 0 then "Even" else "Odd"

let () = printf "%s\n%!" ans
