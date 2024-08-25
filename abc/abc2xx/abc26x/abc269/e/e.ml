open Core
open Scanf

let n = scanf "%d" ident

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok left mid then left, mid else mid, right in
    binsearch ~ok left right

let question (a, b) (c, d) = printf "? %d %d %d %d\n%!" a b c d; scanf " %d" ident

let all = 1, n

let i = binsearch 0 n ~ok:(fun u m -> question (u+1, m) all <> m - u)
let j = binsearch 0 n ~ok:(fun l m -> question all (l+1, m) <> m - l)

let () = printf "! %d %d\n%!" i j
