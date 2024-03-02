open Core
open Scanf

let n = scanf "%Ld" Fn.id

let ans =
  Iter.(1_000_000 --^ 1)
  |> Iter.map Int64.of_int
  |> Iter.map Int64.(fun x -> x * x * x)
  |> Iter.drop_while Int64.(fun x -> x > n)
  |> Iter.map Int64.to_string
  |> Iter.find_pred_exn String.(fun x -> x = rev x)

let () = printf "%s\n%!" ans
