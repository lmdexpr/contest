open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let a =
  Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
  |> Int.Set.of_array |> Set.to_list |> Fn.flip List.take k

let ans = 
  let rec solve acc = function
    | x :: xs when acc + 1 = x -> solve x xs
    | _                        -> acc + 1
  in
  solve (-1) a

let () = printf "%d\n%!" ans
