open Core
open Scanf

let n, m, k = scanf "%Ld %Ld %Ld" Tuple3.create
open Int64

let rec binsearch ~ok left right =
  if abs (right - left) <= 1L then right
  else
    let mid = (right + left) / 2L in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let rec gcd =function
  | 0L, b -> b
  | a, 0L -> a
  | a,  b -> gcd @@ if a < b then a, (b % a) else (a % b), b
let gcd = gcd (n, m)

let lcm = (n * m) / gcd

let f x = x / m + x / n - x / lcm * 2L
let ok x = f x >= k

let ans = binsearch ~ok 0L 2_000_000_000_000_000_000L

let () =
  printf "%Ld\n%!" ans
