open Core
open Scanf

let s = scanf " %s" Fun.id

let yes = String.for_all s ~f:Char.(fun c -> c = s.[0])

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
