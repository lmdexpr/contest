open Core
open Scanf

let n = scanf "%d" Fn.id
let w = Array.init n ~f:(fun _ -> scanf " %s" Fn.id) |> String.Set.of_array

let yes = List.exists ~f:(Set.mem w) [ "and"; "not"; "that"; "the"; "you" ]

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
