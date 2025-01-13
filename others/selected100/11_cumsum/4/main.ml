open Core
open Scanf

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init ~f:(paired f)

let n, m, q = scanf "%d %d %d" Tuple3.create

let trains = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) 0
let () =
  for _ = 1 to m do
    let l, r = scanf " %d %d" Tuple2.create in
    trains.(l).(r) <- trains.(l).(r) + 1
  done;
  for l = 1 to n do
    trains.(l) <- cumsum ~init:0 ~f:(+) trains.(l)
  done

let () =
  for _ = 1 to q do
    scanf " %d %d" @@ fun p q ->

    Iter.(p -- q)
    |> Iter.fold (fun acc l -> acc + trains.(l).(q)) 0
    |> printf "%d\n"
  done
