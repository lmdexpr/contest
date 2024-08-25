open Core
open Scanf

let s = scanf "%s" Fn.id
let n = String.length s

let yes =
  Char.(s.[0] = '<' && s.[n - 1] = '>') &&
  Iter.(1 -- (n - 2)) |> Iter.for_all Char.(fun i -> s.[i] = '=')

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
