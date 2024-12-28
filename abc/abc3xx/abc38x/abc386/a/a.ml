open Core
open Scanf

let a, b = scanf  "%d %d" Tuple2.create
let c, d = scanf " %d %d" Tuple2.create

let yes =
  Int.Set.of_list [a; b; c; d]
  |> Set.length

let ans = if yes = 2 then "Yes" else "No"

let () = printf "%s\n%!" ans
