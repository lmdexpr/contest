open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create
let a    = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes = Array.find a ~f:(fun i -> i = x) |> Option.is_some

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
