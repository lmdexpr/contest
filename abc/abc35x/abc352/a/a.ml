open Core
open Scanf

let _ = scanf "%d" Fn.id
let x, y, z = scanf " %d %d %d" Tuple3.create

let yes =
  if x < y then 
    x < z && z < y
  else if x > y then
    y < z && z < x
  else
    x = y && y = z

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
