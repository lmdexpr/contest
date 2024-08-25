open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ans = Array.sum (module Int) a ~f:ident

let () = printf "%d\n%!" ans
