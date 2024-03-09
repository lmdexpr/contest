open Core
open Scanf

let a, b = scanf "%s %s" Tuple2.create
let n = a ^ b |> Int.of_string

let ans =
  Iter.(1 -- n)
  |> Iter.filter (fun x -> x * x <= n)
  |> Iter.exists (fun x -> x * x = n)

let ans = if ans then "Yes" else "No"

let () = printf "%s\n%!" ans
