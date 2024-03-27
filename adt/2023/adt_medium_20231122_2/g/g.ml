open Core
open Scanf

let n = scanf "%d" Fn.id

let sx, sy = scanf " %d %d" Tuple2.create
let tx, ty = scanf " %d %d" Tuple2.create

let pt = Array.init n ~f:(fun _ -> scanf " %d %d %d" Tuple3.create)

let uf = Array.init (n+2) ~f:Union_find.create

let () =
  for i = 1 to n do
    let x, y, r = pt.(i - 1) in
    let collision (x', y', r') =
      let p2 x = x * x in
      let d = p2 (x - x') + p2 (y - y') in
      d <= p2 (r + r') && d >= p2 (r - r')
    in

    if collision (sx, sy, 0) then
      Union_find.union uf.(0) uf.(i);
    if collision (tx, ty, 0) then
      Union_find.union uf.(i) uf.(n+1);

    for j = 1 to n do
      if collision pt.(j - 1) then
        Union_find.union uf.(i) uf.(j)
    done
  done

let yes = Union_find.same_class uf.(0) uf.(n+1)

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
