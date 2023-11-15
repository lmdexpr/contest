open Core
open Scanf

let n, a, b = scanf "%Ld %Ld %Ld" Tuple3.create

open Int64
let rec gcd =
  function 
  | 0L, b -> b
  | a, 0L -> a
  | a, b -> gcd @@ if a < b then a, (b % a) else (a % b), b
let gcd = Tuple2.curry gcd

let lcm a b = a * b / gcd a b

let sum n = n * (n + 1L) / 2L

let lcm = lcm a b
let ans = sum n - a * sum (n / a) - b * sum (n / b) + lcm * sum (n / lcm)

let () = printf "%Ld\n%!" ans
