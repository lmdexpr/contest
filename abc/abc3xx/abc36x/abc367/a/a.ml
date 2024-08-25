open Core
open Scanf

let a, b, c = scanf "%d %d %d" Tuple3.create

let yes = 
  if c < b then
    c < a && a < b
  else
    not (b < a && a < c)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
