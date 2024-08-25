open Core
open Scanf

let rec gcd =function
  | 0, b -> b
  | a, 0 -> a
  | a, b -> gcd @@ if a < b then a, (b % a) else (a % b), b
let gcd = Tuple2.curry gcd

let solve n d k = (k - 1) / (n / gcd n d) + (k - 1) * d % n

let t = scanf "%d" ident
let () =
  for _ = 1 to t do
    scanf " %d %d %d" solve |> printf "%d\n%!"
  done
