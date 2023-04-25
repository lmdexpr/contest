open Core
open Scanf

let n = scanf "%d" ident

let ans = 1000 - n % 1000
let ans = if ans = 1000 then 0 else ans

let () = printf "%d\n%!" ans
