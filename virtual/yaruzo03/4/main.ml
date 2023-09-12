open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let x = Array.init m ~f:(fun _ -> scanf " %d" ident)

let () = if m <= n then (printf "0\n%!"; exit 0)

let () = Array.sort x ~compare

let edge = Array.init (m - 1) ~f:(fun i -> x.(i + 1) - x.(i))
let () = Array.sort edge ~compare

let ans = Iter.(0 -- (m - n - 1)) |> Iter.map (Array.get edge) |> Iter.sum

let () = printf "%d\n%!" ans
