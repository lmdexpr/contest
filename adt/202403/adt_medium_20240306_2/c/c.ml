open Core
open Scanf

let s, t = scanf "%d %d" Tuple2.create

let ans =
  Iter.(0 -- 100) |> Iter.flat_map (fun a ->
  Iter.(0 -- (100 - a)) |> Iter.flat_map (fun b ->
  Iter.(0 -- (100 - a - b)) |> Iter.filter (fun c ->
    a + b + c <= s && a * b * c <= t
  )))
  |> Iter.length

let () = printf "%d\n%!" ans
