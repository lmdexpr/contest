open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id) |> Int.Set.of_array

let yes = Set.exists a ~f:(fun ai -> Set.mem a (ai - x))
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
