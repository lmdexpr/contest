open Core
open Scanf

let s = scanf "%s" Fn.id

let yes = 
  not (String.for_all s ~f:Char.is_uppercase) &&
  not (String.for_all s ~f:Char.is_lowercase) &&
  Char.Set.of_list (String.to_list s) |> Set.length = String.length s

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
