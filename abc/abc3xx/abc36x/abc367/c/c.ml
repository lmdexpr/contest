open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let r = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let rec generate i acc =
  if n = i then acc
  else
    Iter.flat_map (fun acc ->
      Iter.(1 -- r.(i)) |> Iter.map (fun j -> j :: acc)
    ) acc
    |> generate (i + 1)

let () =
  generate 0 Iter.(singleton [])
  |> Iter.filter (fun seq -> List.sum (module Int) seq ~f:Fn.id % k = 0)
  |> Iter.iter (fun seq ->
    List.rev seq
    |> List.iter ~f:(printf "%d ");
    printf "\n"
  )

