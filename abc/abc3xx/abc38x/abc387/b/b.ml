open Core
open Scanf

let x = scanf "%d" Fn.id

let ans =
  Iter.(product (1 -- 9) (1 -- 9))
  |> Iter.filter (fun (a, b) -> a * b = x)
  |> Iter.fold   (fun acc _ -> acc - x) 2025

let () = printf "%d\n%!" ans
