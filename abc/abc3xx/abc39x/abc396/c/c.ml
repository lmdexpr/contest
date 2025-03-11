open Core
open Scanf

let sum = List.sum (module Int) ~f:Fn.id

let n, m = scanf " %d %d" Tuple2.create

let compare = Int.descending

let b = List.init n ~f:(fun _ -> scanf " %d" Fn.id) |> List.sort ~compare
let w = List.init m ~f:(fun _ -> scanf " %d" Fn.id) |> List.sort ~compare

let bp, bn = List.split_while b ~f:(fun x -> x >= 0)
let wp, _  = List.split_while w ~f:(fun x -> x >= 0)

let k = List.length bp

let wp_k, rest = List.split_n wp k

let rec solve acc = function
  | ([], _) | (_, [])  -> acc
  | (b :: bn, w :: wp) ->
    if b + w <= 0 then acc
    else
      solve (acc + b + w) (bn, wp)

let ans = solve (sum bp + sum wp_k) (bn, rest)

let () = printf "%d\n%!" ans
