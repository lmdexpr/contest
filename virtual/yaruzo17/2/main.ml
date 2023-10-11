open Core
open Scanf

let n = scanf "%d" Fn.id

let ans =
  Iter.(1 -- 50000)
  |> Iter.find_pred (fun x -> Float.to_int (float x *. 1.08) = n)

let () =
  match ans with
  | None     -> printf ":(\n"
  | Some ans -> printf "%d\n%!" ans
