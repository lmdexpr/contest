open Core
open Scanf

let n = scanf "ABC%d" Fn.id

let ans = if 0 < n && n < 350 && n <> 316 then "Yes" else "No"

let () = printf "%s\n%!" ans
