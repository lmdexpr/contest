open Core

let a, b = Scanf.scanf "%Ld %Ld" Tuple2.create

let rec gcd =
  let open Int64 in
  function 
  | 0L, b -> b
  | a, 0L -> a
  | a, b -> gcd @@ if a < b then a, (b % a) else (a % b), b
let gcd = Tuple2.curry gcd

(* let lcm a b = let open Int64 in a * b / gcd a b *)

let pow18 = 1000000000000000000L

let () =
  let open Int64 in
  let r = b / gcd a b in
  if r > pow18 / a then print_endline "Large"
  else
    printf "%Ld\n%!" (r * a)
