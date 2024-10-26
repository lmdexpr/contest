open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let d = Array.create ~len:(m+1) 1
let () =
  for _ = 1 to n do
    let l, r = scanf " %d %d" Tuple2.create in
    d.(r) <- max d.(r) (l + 1)
  done
let () =
  for i = 1 to m do
    d.(i) <- max d.(i) d.(i-1)
  done

let ans =
  Iter.(1 -- m)
  |> Iter.map (fun r -> r - d.(r) + 1)
  |> Iter.sum

let () = printf "%d\n%!" ans
