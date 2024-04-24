open Core
open Scanf

let a, b, c = scanf "%d %d %d" Tuple3.create

let ans =
  Iter.(a -- b)
  |> Iter.find_pred (fun x -> x mod c = 0)
  |> Option.value ~default:(-1)

let () = printf "%d\n%!" ans
