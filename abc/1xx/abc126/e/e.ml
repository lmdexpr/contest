open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let uf = Array.init n ~f:Union_find.create
let () =
  for _ = 1 to m do
    let x, y, _z = scanf " %d %d %d" Tuple3.create in
    Union_find.union uf.(x - 1) uf.(y - 1)
  done

let ans = Array.counti uf ~f:(fun i uf -> Union_find.get uf = i)

let () = printf "%d\n%!" ans
