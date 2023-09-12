open Core
open Scanf

let n = scanf "%Ld" ident

let rec binsearch ~ok left right =
  let open Int64 in
  if abs (right - left) <= 1L then right
  else
    let mid = (right + left) / 2L in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans = binsearch ~ok:Int64.(fun k -> k * (k + 1L) / 2L <= n + 1L) 2_000_000_000L 0L
let ans = Int64.(n + 1L - ans)

let () = printf "%Ld\n%!" ans
