open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init k ~f:(fun _ -> scanf " %d" Int.pred)

let xy = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let mat = Array.make_matrix ~dimx:n ~dimy:k 0.
let () =
  for i = 0 to n - 1 do
    for j = 0 to k - 1 do
      let xi, yi = xy.(i) in
      let xj, yj = xy.(a.(j)) in
      mat.(i).(j) <- sqrt @@ float @@ (xi - xj) * (xi - xj) + (yi - yj) * (yi - yj)
    done
  done

let ans = 
  Array.map mat ~f:(Array.fold ~init:Float.max_finite_value ~f:Float.min)
  |> Array.fold ~init:0.0 ~f:Float.max

let () = printf "%f\n%!" ans
