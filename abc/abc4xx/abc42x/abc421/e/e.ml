open Core
open Scanf

let n = scanf " %d" Fn.id

let _a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = 0

let () = printf "%d\n%!" ans
