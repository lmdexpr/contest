open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let count = Array.init 100_001 ~f:(const 0)

let () =
  for _ = 1 to n do
    let a, b = scanf " %d %d" Tuple2.create in
    count.(a) <- count.(a) + b
  done

let ans =
  Iter.of_array_i count
  |> Iter.fold_while
    (fun acc (i, c) ->
       if acc <= c then i, `Stop
       else
         acc - c, `Continue
    )
    k

let () = printf "%d\n%!" ans
