open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let inf = 1_000_000_001
let ans =
  List.init m ~f:Fn.id
  |> List.folding_map ~init:1 ~f:(fun acc _ ->
    if acc = inf then inf, inf
    else if n >= inf / acc then inf, inf
    else
      let acc = min inf @@ acc * n in
      acc, acc
  )
  |> List.fold ~init:1 ~f:(fun acc x ->
    if acc = inf || x = inf then inf
    else if x >= inf - acc then inf
    else
      acc + x
  )

let () = 
  if ans = inf then
    printf "inf\n%!"
  else
    printf "%d\n%!" ans
