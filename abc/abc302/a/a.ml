open Core
open Scanf

let a, b = scanf "%Ld %Ld" Tuple2.create

let ans = Int64.(a / b + if a % b <> 0L then 1L else 0L)

let () = printf "%Ld\n%!" ans
