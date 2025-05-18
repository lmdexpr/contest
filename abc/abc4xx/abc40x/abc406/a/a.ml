open Core
open Scanf

let a = scanf " %d" Fn.id
let b = scanf " %d" Fn.id
let c = scanf " %d" Fn.id
let d = scanf " %d" Fn.id

let yes =
  c < a || (c = a && d <= b)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
