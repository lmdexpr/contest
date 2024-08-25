open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let f _ =
  let c = scanf " %d" ident in
  Iter.(1 -- c)
  |> Iter.fold
    (fun acc _ ->
       let a = scanf " %d" ident in
       acc lor (1 lsl (a - 1))
    )
    0
let a = Array.init m ~f

let ans =
  Iter.(1 -- (1 lsl m - 1))
  |> Iter.map (fun mask ->
      Iter.(0 -- (m - 1))
      |> Iter.filter (fun i -> mask land (1 lsl i) <> 0)
      |> Iter.map (Array.get a)
      |> Iter.fold (fun acc a -> acc lor a) 0
    )
  |> Iter.filter_count (fun choiced -> choiced = 1 lsl n - 1)

let () = printf "%d\n%!" ans
