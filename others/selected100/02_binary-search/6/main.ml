(* https://atcoder.jp/contests/joi2008ho/tasks/joi2008ho_c *)
open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let p = Array.init (n+1) ~f:(const 0)
let () = for i = 1 to n do scanf " %d" (Array.set p i) done

let q =
  Iter.of_array p |> Iter.flat_map (fun pi ->
      Iter.of_array p |> Iter.map (fun pj -> pi + pj)
    )
  |> Iter.to_array
let () = Array.sort ~compare q

let ans =
  Iter.of_array q |> Iter.filter_map (fun qi ->
      Array.binary_search ~compare q `Last_less_than_or_equal_to (m-qi)
      |> Option.map ~f:(fun j -> qi + q.(j))
    )
  |> Iter.max ~lt:(<)
  |> Option.value ~default:0

let () = printf "%d\n%!" ans
