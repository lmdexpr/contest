open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.filter (fun i -> i % 2 = 0)
  |> Iter.map (fun i -> a.(i))
  |> Iter.sum

let () = printf "%d\n%!" ans
