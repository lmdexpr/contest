open Core
open Scanf

let p = scanf " %s" Fn.id
let l = scanf " %d" Fn.id

let yes = l <= String.length p

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
