open Core
open Scanf

let n, m = scanf "%d %d\n" Tuple2.create

let uf = Array.init (n+1) ~f:Union_find.create
let () =
  for _ = 1 to m do
    let a, b = scanf "%d %d\n" Tuple2.create in
    Union_find.union uf.(a) uf.(b)
  done

let ans =
  Iter.(1 -- n)
  |> Iter.filter (fun i -> i = Union_find.get uf.(i))
  |> Iter.length

let () = printf "%d\n%!" ans
