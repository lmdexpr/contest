open Core
open Scanf

let n = scanf "%s" String.to_array
let () =
  Array.sort n ~compare:Char.compare

let yes = String.(of_array n = "122333")

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
