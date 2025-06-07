open Core
open Scanf

let n = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let nodes = Array.create ~len:3_000_000 (0, 0)
let () =
  for i = 1 to n do
    let a, b = scanf " %d %d" Tuple2.create in
    nodes.(i) <- a, b
  done

let dist i j =
  let x1, y1 = nodes.(i) in
  let x2, y2 = nodes.(j) in
  abs (x1 - x2) + abs (y1 - y2)

let n = ref n and m = ref n

let uf = Array.init 3_000_000 ~f:Union_find.create

let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let a, b = scanf " %d %d" Tuple2.create in
      nodes.(!n) <- a, b;
      incr n;
    | 2 when !m = 1 -> printf "-1\n%!"
    | 2 ->
      ()
    | _ -> 
      let u, v = scanf " %d %d" Tuple2.create in
      printf "%s\n%!" @@
      if Union_find.same_class uf.(u) uf.(v) then "Yes" else "No";
  done
