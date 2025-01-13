open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

let es = Array.init m ~f:(fun _ -> scanf " %d %d" @@ fun a b -> a-1, b-1)

let connected idxes = 
  let uf = Array.init n ~f:Union_find.create in
  idxes |> Iter.iter (fun i -> 
    es.(i) |> Tuple2.map ~f:(Array.get uf) |> Tuple2.uncurry Union_find.union
  );
  Array.for_all uf ~f:(Union_find.same_class uf.(0))

let ans =
  Iter.(0 -- (m - 1))
  |> Iter.filter_count (fun i -> 
    Iter.(0 -- (m - 1)) |> Iter.filter (fun j -> i <> j) |> connected |> not
  )

let () = printf "%d\n%!" ans
