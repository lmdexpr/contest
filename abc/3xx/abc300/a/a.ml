open Core
open Scanf

let n, a, b = scanf "%d %d %d" Tuple3.create
let c = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ans = Array.findi_exn c ~f:(fun _ c -> a + b = c) |> Tuple2.get1
let ans = ans + 1

let () = printf "%d\n%!" ans
