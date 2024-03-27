open Core
open Scanf

let s = scanf "%s" String.to_list

let yes =
  let rec a = function
    | []          -> true
    | 'C' :: rest -> c rest
    | 'B' :: rest -> b rest
    | 'A' :: rest -> a rest
    | _           -> false
  and b = function
    | []          -> true
    | 'C' :: rest -> c rest
    | 'B' :: rest -> b rest
    | _           -> false
  and c = function
    | []          -> true
    | 'C' :: rest -> c rest
    | _           -> false
  in
  a s

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
