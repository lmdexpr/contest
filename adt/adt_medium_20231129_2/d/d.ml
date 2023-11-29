open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id) |> Int.Set.of_array

let ans = Set.length a

let () = printf "%d\n%!" ans
