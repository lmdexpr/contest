open Core
open Scanf

let n, k = scanf "%Ld %d" Tuple.T2.create

let rec solve n i =
  if i <= 0 then n
  else
    let i = i - 1 in
    if Int64.(n % 200L = 0L) then
      solve Int64.(n / 200L) i
    else
      solve Int64.(n * 1000L + 200L) i

let ans = solve n k

let () = printf "%Ld\n%!" ans
