open Core
open Scanf

let x, y = scanf "%d %d" Tuple2.create

let f = function
  | 3 -> 100000
  | 2 -> 200000
  | 1 -> 300000
  | _ -> 0

let ans = f x + f y + if x = 1 && y = 1 then 400000 else 0

let () = printf "%d\n%!" ans
