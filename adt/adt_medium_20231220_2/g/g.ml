open Core
open Scanf

let a, b = scanf "%Ld %Ld" Tuple2.create
let a = Int64.to_float a
let b = Int64.to_float b 

let g_sqrtg = a /. (2. *. b)
let g = Float.(g_sqrtg ** (2. /. 3.))

let f x = b *. x +. a /. Float.sqrt (x +. 1.)

let fx_up = f @@ Float.round_up g
let fx_dn = f @@ Float.round_down g

let ans = Float.min fx_up fx_dn
let () = printf "%.10f\n%!" ans
