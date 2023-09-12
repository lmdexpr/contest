open Core
open Scanf

let a, b, k = scanf "%d %d %d" Tuple3.create

let ans =
  Iter.(Int.min a b --^ 1)
  |> Iter.filter (fun x -> a % x = 0 && b % x = 0)
  |> Iter.drop (k - 1) |> Iter.head_exn

let () = printf "%d\n%!" ans
