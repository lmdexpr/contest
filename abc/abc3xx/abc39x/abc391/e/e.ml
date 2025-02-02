open Core
open Scanf

let n = scanf " %d" Fn.id
let s = scanf " %s" String.to_array 
  |> Array.map ~f:(function '1' -> true | _ -> false)

let rec dfs n l =
  if n = 0 then
    if s.(l) then (0, 1) else (1, 0)
  else
    Iter.(0 -- 2)
    |> Iter.map (fun k -> dfs (n - 1) Int.(l + k * 3 ** (n - 1)))
    |> Iter.fold
      (fun (sz, mz, so, mo) (zero, one) ->
        sz + zero, max mz zero, so + one, max mo one
      )
      (0, 0, 0, 0)
    |> fun (sumz, maxz, sumo, maxo) ->
    sumz - maxz, sumo - maxo


let ans = Tuple2.uncurry max @@ dfs n 0

let () = printf "%d\n%!" ans
