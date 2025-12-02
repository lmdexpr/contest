open Core
open Scanf


let n = scanf "%f" Fn.id

let rec binsearch ~ok left right =
  let open Float in
  if abs (right - left) <= 0.0001 then right
  else
    let mid = (right + left) / 2. in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans = binsearch 101. 1.
  ~ok:Float.(fun x -> x * x * x + x <= n)

let () = printf "%.6f\n%!" ans
