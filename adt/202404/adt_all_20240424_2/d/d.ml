open Core
open Scanf

let s = scanf "%s" Fn.id

let yes = Str.(string_match (regexp "^A*B*C*$") s 0)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
