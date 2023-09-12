open Core
open Scanf

let x = scanf "%d" ident

let ans =
  Iter.(1 -- x)
  |> Iter.scan (+) 0
  |> Iter.findi (fun i n -> Option.some_if (n >= x) i)
  |> Option.value ~default:0

let () = printf "%d\n%!" ans
