open Core
open Scanf

let n = scanf "%d" ident

let a = n / 10 * 10
let b = a + 5

let ans = if abs (n - a) < abs (b - n) then a else b

let () = printf "%d\n%!" ans
