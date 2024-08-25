open Core
open Scanf

let _n, q = scanf "%d %d" Tuple2.create

let () =
  Iter.(1 -- q) |> Iter.fold
    (fun (i, called) _ -> 
       match scanf " %d" ident with
       | 1 -> let i = i + 1 in i, Int.Set.add called i
       | 2 -> let x = scanf " %d" ident in i, Int.Set.remove called x
       | _ -> printf "%d\n" @@ Int.Set.min_elt_exn called; i, called
    )
    (0, Int.Set.empty)
  |> ignore
