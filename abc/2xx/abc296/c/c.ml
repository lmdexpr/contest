open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let origin = Int.Set.of_array a
let plus_x = Int.Set.of_array (Array.map a ~f:((+) x))

let ans = if Int.Set.inter origin plus_x |> Int.Set.is_empty then "No" else "Yes"

let () = printf "%s\n%!" ans
