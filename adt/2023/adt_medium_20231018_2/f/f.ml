open Core
open Scanf

let n = scanf "%d" Fn.id
let ab = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let time = Array.map ab ~f:(fun (a, b) -> float a /. float b)
let t = Array.sum (module Float) time ~f:Fn.id
let t = t /. 2.

let rec simulate t i left =
  if Float.(t -. time.(i) <= 0.) then t, i, float left
  else 
    simulate (t -. time.(i)) (i + 1) (left + Tuple2.get1 ab.(i))

let t, i, left = simulate t 0 0
let ans = left +. t *. float (Tuple2.get2 ab.(i))

let () = printf "%.10f\n%!" ans
