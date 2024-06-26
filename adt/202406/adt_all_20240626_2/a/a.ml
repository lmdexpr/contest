open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes = Int.Set.of_array a |> Set.length = 1

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
