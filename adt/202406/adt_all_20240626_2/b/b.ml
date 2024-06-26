open Core
open Scanf

let n = scanf "%d" Fn.id

let yes = not (2 <= n && n <= 4)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
