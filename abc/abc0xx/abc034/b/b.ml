open Core
open Scanf

let n = scanf "%d" ident

let ans = n + if n % 2 = 0 then -1 else 1

let () = printf "%d\n%!" ans
