open Core
open Scanf

let _n, d = scanf "%d %d" Tuple2.create
let s = scanf " %s" Fn.id

let ans = String.count s ~f:Char.((=) '.') + d

let () = printf "%d\n%!" ans
