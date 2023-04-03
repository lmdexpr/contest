open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let ans = if a < 10 && b < 10 then a * b else -1

let () = printf "%d\n%!" ans
