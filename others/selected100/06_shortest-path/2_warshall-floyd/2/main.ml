(* https://atcoder.jp/contests/abc012/tasks/abc012_4 *)
open Core
open Scanf

let inf = 10_000_000_000
let n, m = scanf "%d %d" Tuple2.create

let dist = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) inf
let () =
  for _ = 1 to m do
    scanf " %d %d %d" @@ fun a b t ->
    dist.(a).(b) <- t;
    dist.(b).(a) <- t;
  done;
  for i = 1 to n do
    dist.(i).(i) <- 0
  done

let _warshall_floyd =
  for k = 1 to n do
    for i = 1 to n do
      for j = 1 to n do
        if dist.(i).(k) < inf && dist.(k).(j) < inf then
          dist.(i).(j) <- min dist.(i).(j) (dist.(i).(k) + dist.(k).(j))
      done
    done
  done

let ans =
  Array.map dist ~f:(fun dist ->
      Array.filter dist ~f:(fun x -> x < inf)
      |> Array.max_elt ~compare
      |> Option.value ~default:inf
    )
  |> Array.min_elt ~compare
  |> Option.value ~default:inf

let () = printf "%d\n" ans
