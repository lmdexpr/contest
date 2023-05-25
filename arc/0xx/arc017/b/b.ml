open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let rec solve ?(acc=Iter.empty) ?(c=1) i =
  if i >= n then Iter.snoc acc c
  else if a.(i-1) < a.(i) then solve ~acc ~c:(c+1) (i+1)
  else
    solve ~acc:(Iter.snoc acc c) ~c:1 (i+1)

let ans = solve 1 |> Iter.map (fun c -> c - k + 1) |> Iter.filter (fun c -> c > 0) |> Iter.sum

let () = printf "%d\n%!" ans
