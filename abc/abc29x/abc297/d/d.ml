open Core
open Scanf

open Int64

let rec solve ?(acc=0L) a b =
  if a = b then acc
  else
    let k = b / a in
    if b - (k - 1L) * a = a then acc + k - 1L
    else
      solve ~acc:(acc + k) (b - k * a) a

let a, b = scanf "%Ld %Ld" Tuple2.create
let a, b = min a b, max a b
let ans = solve a b
let () = printf "%Ld\n%!" ans
