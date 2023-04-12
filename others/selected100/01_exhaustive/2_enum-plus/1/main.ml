(* https://atcoder.jp/contests/abc095/tasks/arc096_a *)

open Core
open Scanf

let a, b, c = scanf "%d %d %d" Tuple3.create
let x, y = scanf " %d %d" Tuple2.create

let c = 2 * c

let ans =
  Iter.(0 -- Int.max x y)
  |> Iter.map (fun ab -> Int.max 0 (x - ab), Int.max 0 (y - ab), ab)
  |> Iter.map (fun (x, y, z) -> a * x + b * y + c * z)
  |> Iter.min_exn ~lt:(<)

let () = printf "%d\n%!" ans
