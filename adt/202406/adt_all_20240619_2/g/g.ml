open Core
open Scanf

let a, b, c' = scanf "%d %d %Ld" Tuple3.create
let c = Int64.popcount c'

let ans = 0

let () = printf "%d\n%!" ans
