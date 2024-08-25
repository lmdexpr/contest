open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let rec combinations ?(acc=Iter.empty) = function
  | 0 -> const @@ Iter.singleton Iter.empty
  | k -> generate acc k
and
  generate acc len iter = match Iter.head iter, Iter.drop 1 iter with
  | None, _     -> acc
  | Some hd, tl -> 
    let acc = combinations (len - 1) tl |> Iter.fold (fun acc cs -> Iter.snoc acc (Iter.cons hd cs)) acc in
    combinations ~acc len tl

let () =
  Iter.(1 -- m)
  |> combinations n 
  |> Iter.iter (fun xs -> Iter.iter (printf "%d ") xs; printf "\n%!")
