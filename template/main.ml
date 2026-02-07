open Core
open Scanf

let n = scanf " %d" Fun.id

let _a = Array.init n ~f:(fun _ -> scanf " %d" Fun.id)

let ans = 0

let () = printf "%d\n%!" ans
