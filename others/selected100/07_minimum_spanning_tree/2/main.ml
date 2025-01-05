open Core
open Scanf

let n, m, k = scanf "%d %d %d" Tuple3.create

type edge = {u : int; v : int; w : int;}

let es = Array.init m ~f:(fun _ -> scanf " %d %d %d" @@ fun u v w -> {u; v; w;})

(* kruskal / クラスカル法 *)
let ans =
  let dsu = Array.init (n+1) ~f:Union_find.create in
  Array.sort es ~compare:(fun e e' -> compare e.w e'.w);
  Array.fold_until es ~init:(0, n) ~finish:fst ~f:(fun (ans, size) {u; v; w;} ->
    let open Continue_or_stop in
    if size = k then Stop ans
    else if Union_find.same_class dsu.(u) dsu.(v) then Continue (ans, size)
    else (
      Union_find.union dsu.(u) dsu.(v); Continue (ans + w, size - 1)
    )
  )

let () = printf "%d\n%!" ans
