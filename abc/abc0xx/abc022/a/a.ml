open Core
open Scanf

let n, s, t = scanf "%d %d %d" Tuple3.create
let w = scanf " %d" ident
let a = Array.init (n - 1) ~f:(fun _ -> scanf " %d" ident)

let ans =
  Iter.of_array a
  |> Iter.scan (+) w
  |> Iter.filter (fun x -> s <= x && x <= t)
  |> Iter.length

let () = printf "%d\n%!" ans
