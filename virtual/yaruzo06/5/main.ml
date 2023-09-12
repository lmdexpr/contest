open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let modulo = 1_000_000_007

let ans = 0

let () = printf "%d\n%!" ans
