open Core
open Scanf

let h, w = scanf "%d %d\n" Tuple2.create
let r, c = scanf "%d %d\n" Tuple2.create

let ans = 
  [ 1, 0; 0, 1; -1, 0; 0, -1 ]
  |> List.map ~f:(fun (dr, dc) -> r + dr, c + dc)
  |> List.filter ~f:(fun (r, c) -> 1 <= r && r <= h && 1 <= c && c <= w)
  |> List.length

let () = printf "%d\n%!" ans
