open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let ans = 
  [ 1; 2; 3 ] |> List.filter ~f:(fun x -> x <> a && x <> b)

let ans =
  match ans with
  | [ x ] -> x
  | _     -> -1

let () = printf "%d\n%!" ans
