open Core
open Scanf

let h = scanf " %d" Fn.id
let b = scanf " %d" Fn.id

let ans = max 0 (h - b)

let () = printf "%d\n%!" ans
