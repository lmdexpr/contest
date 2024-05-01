open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans1 = Array.zip_exn a b |> Array.count ~f:(fun (a, b) -> a = b)

let () = printf "%d\n%!" ans1

let ans2 = 
  Iter.of_array_i a
  |> Iter.flat_map (fun (i, a) -> Iter.of_array_i b |> Iter.map (fun (j, b) -> i, a, j , b))
  |> Iter.filter_count (fun (i, a, j, b) -> i <> j && a = b)

let () = printf "%d\n%!" ans2
