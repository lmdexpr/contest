open Core
open Scanf

let n = scanf "%d" ident
let a, b = scanf " %d %d" Tuple2.create

let n = n % (a + b)

let ans = if n > 0 && n <= a then "Ant" else "Bug"

let () = printf "%s\n%!" ans
