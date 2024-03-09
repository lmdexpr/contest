open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let uf = Array.init n ~f:Union_find.create
let ans =
  Iter.(1 -- m)
  |> Iter.fold
    (fun ans _ ->
       let a, b = scanf " %d %d" @@ fun a b -> a - 1, b - 1 in
       let ans = ans + Bool.to_int (Union_find.same_class uf.(a) uf.(b)) in
       Union_find.union uf.(a) uf.(b);
       ans
    )
    0

let () = printf "%d\n%!" ans
