open Core
open Scanf

let n = scanf "%Ld" ident

let ans = Int64.(n % 998244353L)

let () = printf "%Ld\n%!" ans
