open Core
open Scanf

let a1 = scanf " %d" Fn.id
let a2 = scanf " %d" Fn.id
let a3 = scanf " %d" Fn.id

let yes =
  a1 * a2 = a3 || a1 * a3 = a2 || a2 * a3 = a1

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
