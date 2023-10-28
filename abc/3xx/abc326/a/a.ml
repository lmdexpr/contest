open Core
open Scanf

let x, y = scanf "%d %d" Tuple2.create

let yes = -3 <= y - x && y - x <= 2 

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
