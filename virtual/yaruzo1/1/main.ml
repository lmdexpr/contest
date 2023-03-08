open Core
open Scanf

let s = scanf "%s" ident

let ans = if String.(s = rev s) then "Yes" else "No"

let () = printf "%s\n%!" ans
