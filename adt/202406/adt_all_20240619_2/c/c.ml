open Core
open Scanf

let a, m = scanf "%Ld %Ld" Tuple2.create
let l, r = scanf " %Ld %Ld" Tuple2.create

open Int64

let l, r = l - a, r - a

let (/) a b = (a - (a % b + b) % b) / b

let ans = r / m - (l - 1L) / m

let () = printf "%Ld\n%!" ans
