open Core
open Scanf

let s = scanf "%s" String.to_list

let rec solve acc = function
  | '0' :: '0' :: rest -> solve (acc + 1) rest
  | _ :: rest          -> solve (acc + 1) rest
  | []                 -> acc

let ans = solve 0 s

let () = printf "%d\n%!" ans
