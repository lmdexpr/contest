open Core
open Scanf

let ans = scanf "%d %d" @@ Fn.flip (/)

let () = printf "%d\n%!" ans
