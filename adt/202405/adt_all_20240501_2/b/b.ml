open Core
open Scanf

let x, y, n = scanf "%d %d %d" Tuple3.create

let ans = 
  if x * 3 < y then x * n
  else
    n / 3 * y + n % 3 * x

let () = printf "%d\n%!" ans
