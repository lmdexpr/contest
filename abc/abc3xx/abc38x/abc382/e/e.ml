open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create
let p = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let g = 
  let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0. in
  dp.(0).(0) <- 1.;
  for i = 1 to n do
    let p = p.(i - 1) in
    for j = 1 to n do
      dp.(i).(j) <- 
        dp.(i - 1).(j - 1) *. float p +. 
        dp.(i - 1).(j)     *. (float @@ 100 - p);
      dp.(i).(j) <- 
        dp.(i).(j) /. 100.
    done
  done;
  for i = 0 to n do
    printf "%d: %f\n" i dp.(n).(i)
  done;
  Array.get dp.(n)

let ans =
  let dp = Array.create ~len:(x + 1) 0. in
  for i = 1 to x do
    let f = 
      Iter.(1 -- Int.min n i) 
      |> Iter.fold (fun acc j -> acc +. dp.(i - j) *. g j) 0.
    in
    dp.(i) <- (1. +. f) /. (1. -. g 0);
    printf "%d: %f\n" i dp.(i)
  done;
  dp.(x)

(* sample WA *)
let () = printf "%f\n%!" ans
