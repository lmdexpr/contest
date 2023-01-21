open Core
open Scanf

let n = scanf "%d" ident

let _a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let _scan_1 _ = scanf " %d" ident
let _scan_2 _ = scanf " %d %d" Tuple2.create
let _scan_3 _ = scanf " %d %d %d" Tuple3.create

let ans = 0

let () = printf "%d\n%!" ans
