open Core
open Scanf

let n = scanf "%d" ident

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then left
  else
    let mid = (right + left) / 2 in
    let left, right = if ok left mid then left, mid else mid, right in
    binsearch ~ok left right

let question (a, b) (c, d) = printf "? %d %d %d %d\n%!" a b c d; scanf " %d" ident

let all = 1, n

let i = binsearch 1 (n+1) ~ok:(fun u m -> question (u, m - 1) all <> m - u)
let j = binsearch 1 (n+1) ~ok:(fun l m -> question all (l, m - 1) <> m - l)

let () = printf "! %d %d\n%!" i j
