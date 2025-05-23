open Core
open Scanf

let n = scanf " %Ld" Fn.id

let ans = Int64.(n * pred n / 2L)

let () = printf "%Ld\n%!" ans
