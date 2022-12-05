open Core
open Int64

let a, b, c = Scanf.scanf "%Ld %Ld %Ld" Tuple3.create

let rec gcd =function 
  | 0L, b -> b
  | a, 0L -> a
  | a, b -> gcd @@ if a < b then a, (b % a) else (a % b), b
let gcd = Tuple2.curry gcd

let r = gcd a @@ gcd b c
let c = a / r - 1L + b / r - 1L + c / r - 1L

let () = printf "%Ld\n%!" c
