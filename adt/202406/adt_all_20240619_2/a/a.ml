open Core
open Scanf

let l, r = scanf "%d %d" Tuple2.create

let ans = String.sub "atcoder" ~pos:(l - 1) ~len:(r - l + 1)

let () = printf "%s\n%!" ans
