open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = List.init n ~f:(fun _ -> scanf " %d" Fn.id) |> List.rev

let rec solve ans acc = function
  | []        -> ans + 1
  | a :: rest ->
    if acc + a <= k then
      solve ans (acc + a) rest
    else
      solve (ans + 1) 0 (a :: rest)

let ans = solve 0 0 a

let () = printf "%d\n%!" ans
