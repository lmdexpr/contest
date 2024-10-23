open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let yes = Iter.(a -- b) |> Iter.find_pred (fun x -> 100 % x = 0) |> Option.is_some

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
