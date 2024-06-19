open Core
open Scanf

let s = scanf "%s" Fn.id
let n = String.length s

let ans = String.sub s ~pos:0 ~len:(n - 1) ^ "4"

let () = printf "%s\n%!" ans
