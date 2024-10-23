open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes = 
  Array.existsi a ~f:(fun i p ->
  Array.existsi a ~f:(fun j q ->
  Array.existsi a ~f:(fun k r -> i <> j && j <> k && k <> i && p + q + r = 1000)))

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
