open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create

let ab = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = 0

let () = printf "%d\n%!" ans
