open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.create ~len:n 0
let b = Array.create ~len:n 0
let c = Array.create ~len:n 0
let () =
  for i = 0 to n - 1 do
    scanf " %d %d %d" @@ fun ai bi ci ->
    a.(i) <- ai; b.(i) <- bi; c.(i) <- ci
  done

let dp = Array.make_matrix ~dimx:n ~dimy:3 0

let chmax i j v = dp.(i).(j) <- max dp.(i).(j) v

let () =
  dp.(0).(0) <- a.(0);
  dp.(0).(1) <- b.(0);
  dp.(0).(2) <- c.(0)

let () =
  for i = 1 to n - 1 do
    let pd = dp.(i - 1) in
    chmax i 0 @@ a.(i) + max pd.(1) pd.(2);
    chmax i 1 @@ b.(i) + max pd.(0) pd.(2);
    chmax i 2 @@ c.(i) + max pd.(0) pd.(1);
  done

let ans = Array.fold dp.(n - 1) ~init:0 ~f:max

let () = printf "%d\n%!" ans
