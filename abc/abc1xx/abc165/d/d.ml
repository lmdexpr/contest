open Core
open Scanf

let a, b, n = scanf "%Ld %Ld %Ld" Tuple3.create

open Int64
let f x = a * x / b - a * (x / b)
let ans = f @@ min (b - 1L) n

let () = printf "%Ld\n%!" ans
