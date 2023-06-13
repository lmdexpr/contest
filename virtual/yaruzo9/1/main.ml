open Core
open Scanf

let a, b, c, d = scanf "%d %d %d %d" (fun a b c d -> a, b, c, d)

let ans = min (min a b) (min c d)

let () = printf "%d\n%!" ans
