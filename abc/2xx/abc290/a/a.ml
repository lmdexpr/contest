open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)
let b = Array.init m ~f:(fun _ -> scanf " %d" ident)

let ans = Array.map b ~f:(fun i -> a.(i - 1)) |> Array.sum (module Int) ~f:ident

let () = printf "%d\n%!" ans
