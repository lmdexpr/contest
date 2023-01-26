open Core
open Scanf

let n = scanf "%d" ident

let rec f = function
  | 0 -> 1
  | k -> k * f (k - 1)

let ans = f n

let () = printf "%d\n%!" ans
