open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let ans = a lor b

let () = printf "%d\n%!" ans
