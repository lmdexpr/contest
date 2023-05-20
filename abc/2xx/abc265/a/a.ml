open Core
open Scanf

let x, y, n = scanf "%d %d %d" Tuple3.create

let triple = n / 3
let ans = x * (n % 3) + triple * min (3 * x) y

let () = printf "%d\n%!" ans
