open Core
open Scanf

let s = scanf "%s" Fn.id

let yes = String.contains s 'A' && String.contains s 'B' && String.contains s 'C'

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
