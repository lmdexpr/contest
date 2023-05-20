open Core
open Scanf

let a = scanf "%d %d" Tuple2.create
let b = scanf " %d %d" Tuple2.create
let c = scanf " %d %d" Tuple2.create
let d = scanf " %d %d" Tuple2.create

let is_inferior a b c =
  let( * )(x1, y1) (x2, y2) = x1 * y2 - x2 * y1 in
  let (-) (x1, y1) (x2, y2) = x1 - x2, y1 - y2 in
  (b - a) * (c - b) >= 0

let is_convex points =
  let n = Array.length points in
  Iter.(0 -- (n - 1))
  |> Iter.map (fun i -> points.(i), points.((i + 1) % n), points.((i + 2) %n))
  |> Iter.for_all (Tuple3.uncurry is_inferior)

let ans = if is_convex [| a; b; c; d |] then "Yes" else "No"

let () = printf "%s\n%!" ans
