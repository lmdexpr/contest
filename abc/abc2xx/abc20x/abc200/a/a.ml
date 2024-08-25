open Core
open Scanf

let n = scanf "%d" Fn.id

let ans = n / 100 + Bool.to_int (n % 100 <> 0)

let () = printf "%d\n%!" ans
