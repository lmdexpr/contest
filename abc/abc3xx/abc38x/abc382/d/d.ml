open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let stop level = m - 10 * (n - level)

let rec solve level acc ans x =
  if level = n then
    Iter.snoc acc x
    |> Iter.to_string ~sep:" " Int.to_string
    |> Iter.snoc ans
  else
    let level = level + 1 in
    let acc = Iter.snoc acc x in
    Iter.(x + 10 -- stop level)
    |> Iter.fold (solve level acc) ans

let ans = 
  Iter.(1 -- stop 1)
  |> Iter.fold (solve 1 Iter.empty) Iter.empty
  |> Iter.to_array

let () =
  printf "%d\n" (Array.length ans);
  Array.iter ans ~f:(printf "%s\n")
