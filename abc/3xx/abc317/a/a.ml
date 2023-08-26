open Core
open Scanf

let n, h, x = scanf "%d %d %d" Tuple3.create
let p = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = Array.findi p ~f:(fun _ p -> h + p >= x) |> Option.value_exn |> fst |> (+) 1

let () = printf "%d\n%!" ans
