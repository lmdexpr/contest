open Core
open Scanf

let _n = scanf "%d" ident

let a, b = scanf " %d %d" Tuple2.create
let c, d = scanf " %d %d" Tuple2.create

let yes = match abs (b - c) with
  | 1 -> true
  | 0 -> b > 0 || (a = 0 || d = 0)
  | _ -> false

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
