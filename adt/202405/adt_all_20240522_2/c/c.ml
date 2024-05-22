open Core
open Scanf

let h, m = scanf "%d %d" Tuple2.create

let split x = x / 10, x % 10

let rec solve h m =
  let a, b = split h in
  let c, d = split m in
  let h' = a * 10 + c in
  let m' = b * 10 + d in
  if h' < 24 && m' < 60 then h, m
  else
    let m = (m + 1) % 60 in
    let h = if m = 0 then (h + 1) % 24 else h in
    solve h m

let h, m = solve h m

let () = printf "%d %d\n%!" h m
