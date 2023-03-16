open Core
open Scanf

let x = scanf "%Ld" ident

open Int64

let rec simulate ?(deposit=100L) year =
  if deposit >= x then year
  else
    let deposit = deposit + deposit / 100L in
    simulate ~deposit Int.(year + 1)

let ans = simulate 0

let () = printf "%d\n%!" ans
