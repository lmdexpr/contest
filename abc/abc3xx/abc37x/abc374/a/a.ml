open Core
open Scanf

let s = scanf "%s" Fn.id

let yes = String.is_suffix s ~suffix:"san"

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
