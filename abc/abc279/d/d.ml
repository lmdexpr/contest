open Core

let a, b = Scanf.scanf "%d %d" Tuple2.create
let a = float a 
let b = float b

let x_sqrtx = a /. (2. *. b)
let x = exp @@ 2. /. 3. *. Float.log x_sqrtx

let f x = b *. x +. a /. sqrt (x +. 1.0)

let fx_up = f @@ Float.round_up x
let fx_dn = f @@ Float.round_down x

let () = printf "%f\n%!" @@ Float.min fx_up fx_dn
