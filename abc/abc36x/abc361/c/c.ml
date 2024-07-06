open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let () = Array.sort a ~compare

let ans =
  Iter.((n - k - 1) -- (n - 1))
  |> Iter.map (fun i -> a.(i) - a.(i - (n - k - 1)))
  |> Iter.min_exn ~lt:(<)

let () = printf "%d\n%!" ans
