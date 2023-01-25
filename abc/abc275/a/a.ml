open Core
open Scanf

let n = scanf "%d" ident

let h = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ans =
  Iter.of_array_i h
  |> Iter.max_exn ~lt:(fun (_, l) (_,r) -> l < r)
  |> Tuple2.get1

let ans = ans + 1

let () = printf "%d\n%!" ans
