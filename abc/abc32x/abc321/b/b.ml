open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create

let a = Array.init (n - 1) ~f:(fun _ -> scanf " %d" Fn.id)
let () = Array.sort a ~compare
let a = Iter.of_array a

let ans =
  Iter.(0 -- 100)
  |> Iter.map (fun i ->
    let iter =
      Iter.cons i a
      |> Iter.sort ~cmp:compare
      |> Iter.drop 1
    in
    let sum = Iter.take (Iter.length iter - 1) iter |> Iter.sum in
    i, sum
  )
  |> Iter.filter_map (fun (i, sum) -> Option.some_if (x <= sum) i)
  |> Iter.min ~lt:(<)
  |> Option.value ~default:(-1)

let () = printf "%d\n%!" ans
