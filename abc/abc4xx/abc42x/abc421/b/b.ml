open Core
open Scanf

let x = scanf " %d" Fn.id
let y = scanf " %d" Fn.id

let f x = Int.of_string @@ String.rev @@ Int.to_string x

let rec solve = function
  | 1 -> x
  | 2 -> y
  | n -> f (solve (n - 1) + solve (n - 2))

let ans = solve 10

let () = printf "%d\n%!" ans
