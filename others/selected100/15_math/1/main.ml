open Core
open Scanf

let a = scanf " %Ld" Fn.id
let b = scanf " %Ld" Fn.id
let k = scanf " %Ld" Fn.id

open Int64

let a, b =
  if k < a then a - k, b
  else
    0L, max 0L @@ b - (k - a)

let () = printf "%Ld %Ld\n%!" a b
