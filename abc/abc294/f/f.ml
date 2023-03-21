open Core
open Scanf

let n, m, k = scanf "%d %d %d" Tuple3.create

let taka = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create) |> Iter.of_array
let aoki = Array.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let rec binsearch ~ok left right =
  if Float.(abs (right -. left) <= 0.000000000001) then right
  else
    let mid = (right +. left) /. 2. in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ok x =
  let z = x /. (1. -. x) in
  let v = Array.init m ~f:(fun i -> let c, d = aoki.(i) in float c -. float d *. z) in
  Array.sort ~compare:Float.compare v;
  Iter.map (fun (a, b) -> float a -. float b *. z) taka
  |> Iter.filter_map (fun w -> Array.binary_search ~compare:Float.compare v `First_greater_than_or_equal_to (-. w))
  |> Iter.map (fun num -> m - num)
  |> Iter.sum < k

let ans =binsearch ~ok 0. 1. *. 100.

let () = printf "%.10f\n%!" ans
