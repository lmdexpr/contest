open Core
open Scanf

let a, b, k = scanf "%d %d %d" Tuple3.create

let ans =
  Iter.(1 -- 32)
  |> Iter.fold_while (fun (acc, i) j ->
    if b <= acc then (acc, i), `Stop
    else
      (acc * k, j), `Continue
  ) (a, 0)
  |> Tuple2.get2

let () = printf "%d\n%!" ans
