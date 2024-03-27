open Core
open Scanf

let s = scanf "%s" Fn.id

let yes =
  String.exists s ~f:Char.is_uppercase &&
  String.exists s ~f:Char.is_lowercase &&
  (String.to_array s |> Char.Set.of_array |> Set.length) = String.length s

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
