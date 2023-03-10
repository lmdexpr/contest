open Core
open Scanf

let a = scanf "%d" ident
let b = scanf " %d" ident

let diff = abs @@ a - b
let ans = min diff (10 - diff)

let () = printf "%d\n%!" ans
