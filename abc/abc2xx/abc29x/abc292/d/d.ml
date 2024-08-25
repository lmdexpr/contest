open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let u  = Array.init m ~f:(const 0)
let uf = Array.init n ~f:Union_find.create
let () =
  for i = 0 to m - 1 do
    let ui, vi = scanf " %d %d" @@ fun u v -> u - 1, v - 1 in
    u.(i) <- ui;
    Union_find.union uf.(ui) uf.(vi)
  done

let v = Array.init n ~f:(const 0)
let () =
  for i = 0 to n - 1 do
    let i = Union_find.get uf.(i) in
    v.(i) <- v.(i) + 1
  done

let e = Array.init n ~f:(const 0)
let () =
  for i = 0 to m - 1 do
    let i = Union_find.get uf.(u.(i)) in
    e.(i) <- e.(i) + 1
  done

let yes = if Array.equal (=) v e then "Yes" else "No"
let () = printf "%s\n%!" yes
