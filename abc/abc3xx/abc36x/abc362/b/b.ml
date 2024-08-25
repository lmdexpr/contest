open Core
open Scanf

let a = scanf "%d %d\n" Tuple2.create
let b = scanf "%d %d\n" Tuple2.create
let c = scanf "%d %d\n" Tuple2.create

let right (x1, y1) (x2, y2) (x3, y3) =
  let x1, y1 = x1 - x3, y1 - y3 in
  let x2, y2 = x2 - x3, y2 - y3 in
  x1 * x2 + y1 * y2 = 0 || x1 * x1 + y1 * y1 = x2 * x2 + y2 * y2

let yes = right a b c || right b c a || right c a b

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
