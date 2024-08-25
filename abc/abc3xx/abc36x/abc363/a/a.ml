open Core
open Scanf

let r = scanf "%d" Fn.id

let ans = 100 - r % 100

let () = printf "%d\n%!" ans
