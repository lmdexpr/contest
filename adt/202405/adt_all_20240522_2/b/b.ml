open Core
open Scanf

let l1, r1 = scanf "%d %d" Tuple2.create
let l2, r2 = scanf " %d %d" Tuple2.create

let l = max l1 l2
let r = min r1 r2

let ans = max 0 @@ r - l

let () = printf "%d\n%!" ans
