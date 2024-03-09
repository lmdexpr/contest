open Core
open Scanf

let n, m, p = scanf "%d %d %d" Tuple3.create

let n = n - m

let ans = n / p + 1

let () =
  if n < 0 then printf "0\n%!"
  else
    printf "%d\n%!" ans
