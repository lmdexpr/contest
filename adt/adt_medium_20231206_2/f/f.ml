open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let cards = Array.init (n+1)  ~f:(const [])
let boxes = Array.init 200001 ~f:(const Int.Set.empty)

let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let i, j = scanf " %d %d" Tuple2.create in
      cards.(j) <- i :: cards.(j);
      boxes.(i) <- Set.add boxes.(i) j
    | 2 ->
      let i = scanf " %d" Fn.id in
      cards.(i) <- List.sort cards.(i) ~compare;
      List.iter cards.(i) ~f:(printf "%d "); printf "\n"
    | _ ->
      let i = scanf " %d" Fn.id in
      Set.iter boxes.(i) ~f:(printf "%d "); printf "\n"
  done
