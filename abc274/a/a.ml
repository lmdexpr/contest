open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let ans = float b /. float a

let () = printf "%.3f\n%!" ans
