open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let ans = Array.count a ~f:(fun (a, b) -> a < b)

let () = printf "%d\n%!" ans
