open Core
open Scanf

let s, t = scanf "%s %s" Tuple2.create

let ans = if String.(s < t) then "Yes" else "No"

let () = printf "%s\n%!" ans
