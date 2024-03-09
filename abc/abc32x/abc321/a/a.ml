open Core
open Scanf

let n = scanf "%s" Fn.id

let yes =
  Iter.(1 -- (String.length n - 1))
  |> Iter.map Char.(fun i -> to_int n.[i - 1], to_int n.[i])
  |> Iter.for_all (fun (a, b) -> a > b)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
