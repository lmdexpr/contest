open Core
open Scanf

let w, h = scanf "%d %d" Tuple2.create

let m = 1_000_000_007

let rec fact ?(acc=1) x =
  if x < 1 then acc
  else
    fact ~acc:(acc * x % m) (x - 1)

let rec modinv ?(b = m) ?(u = 1) ?(v = 0) a =
  if b = 0 then (u % m + m) % m
  else
    let t = a / b in
    let a, b = b, a - t * b in
    let u, v = v, u - t * v in
    modinv ~b ~u ~v a

let ans = fact (h + w - 2) * modinv (fact (h - 1) * fact (w - 1) % m) % m

let () = printf "%d\n%!" ans
