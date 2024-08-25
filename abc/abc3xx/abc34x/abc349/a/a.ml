open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init (n-1) ~f:(fun _ -> scanf " %d" Fn.id)

let ans = - Array.sum (module Int) a ~f:Fn.id

let () = printf "%d\n%!" ans
