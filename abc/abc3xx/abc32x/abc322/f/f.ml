open Core
open Scanf

let _n, q = scanf "%d %d" Tuple2.create
let s = scanf " %s" Fn.id

let query = Array.init q ~f:(fun _ -> scanf " %d %d %d" Tuple3.create)

let ans = 0

let () = printf "%d\n%!" ans
