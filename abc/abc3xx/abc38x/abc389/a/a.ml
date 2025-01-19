open Core
open Scanf

let a, b = scanf "%dx%d" Tuple2.create

let ans = a*b

let () = printf "%d\n%!" ans
