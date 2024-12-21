open Core
open Scanf

let a, b, c = scanf "%d %d %d" Tuple3.create

let yes =
  a + b = c ||
  a + c = b ||
  b + c = a ||
  (a = b && b = c)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
