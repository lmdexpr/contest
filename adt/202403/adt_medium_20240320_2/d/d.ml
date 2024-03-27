open Core
open Scanf

let n = scanf "%d" Fn.id

let (//) a b = a / b * b

let ans =
       if n <       1_000 then n
  else if n <      10_000 then n // 10
  else if n <     100_000 then n // 100
  else if n <   1_000_000 then n // 1000
  else if n <  10_000_000 then n // 10000
  else if n < 100_000_000 then n // 100000
  else                         n // 1000000

let () = printf "%d\n%!" ans
