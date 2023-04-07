open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let probability number =
  let rec loop ?(p = 1. /. float n) score =
    if score >= k then p
    else
      loop ~p:(p *. 0.5) (score * 2)
  in
  loop number

let ans = Iter.(1 -- n) |> Iter.map probability |> Iter.fold (+.) 0.

let () = printf "%.10f\n%!" ans
