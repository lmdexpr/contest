open Core
open Scanf

let ha, wa = scanf "%d %d" Tuple2.create
let a = Array.init ha ~f:(fun _ -> scanf " %s" String.to_array)

let hb, wb = scanf "%d %d" Tuple2.create
let b = Array.init hb ~f:(fun _ -> scanf " %s" String.to_array)

let hx, wx = scanf "%d %d" Tuple2.create
let x = Array.init hx ~f:(fun _ -> scanf " %s" String.to_array)

let ans = 0

let () = printf "%d\n%!" ans
