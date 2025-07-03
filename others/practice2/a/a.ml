open Core
open Scanf

let n = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let dsu = Array.init n ~f:Union_find.create

let () =
  for _ = 1 to q do
    match scanf " %d %d %d" Tuple3.create with
    | 0, u, v -> Union_find.union dsu.(u) dsu.(v)
    | _, u, v -> printf "%d\n" @@ Bool.to_int Union_find.(same_class dsu.(u) dsu.(v))
  done;
