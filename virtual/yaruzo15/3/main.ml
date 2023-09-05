open Core
open Scanf

let _n, q = scanf "%d %d" Tuple2.create

let solve (i, called) = function
  | 1 -> let i = i + 1 in i, Set.add called i
  | 2 -> let x = scanf " %d" Fn.id in i, Set.remove called x
  | _ -> printf "%d\n" (Set.min_elt_exn called); i, called

let () =
  Iter.(1 -- q)
  |> Iter.map (fun _ -> scanf " %d" Fn.id)
  |> Iter.fold solve (0, Int.Set.empty)
  |> ignore
