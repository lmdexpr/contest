open Core
open Scanf

let n, k, q = scanf "%d %d %d" Tuple3.create

let xy = Array.init q ~f:(fun _ -> scanf "%d %d" Tuple2.create)

let _ans =
  Iter.of_array xy
  |> Fn.flip Iter.scan (0, 0) (fun (a, b) (c, d) ->
      if c = a then (a, b + d) else (c, d)
      )
  |> Iter.to_array

let () =
  for i = 1 to q do
    printf "%d\n%!" 0
  done
