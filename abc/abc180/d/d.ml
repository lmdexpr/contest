open Core

let x, y = Scanf.scanf "%Ld %Ld" Tuple2.create
let a, b = Scanf.scanf " %Ld %Ld" Tuple2.create
    
open Int64

let rec solve ?(exp=0L) str =
  if str >= y / a || str >= (str + b) / a then (y - 1L - str) / b + exp
  else
    solve ~exp:(exp + 1L) (str * a)

let () = printf "%Ld\n%!" @@ solve x
