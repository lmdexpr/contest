open Core
open Scanf

let n = scanf "%d" ident
let s = scanf " %s" ident

let t = String.count s ~f:Char.(fun c -> c = 'T')

let ans = if t > n - t || (t = n - t && Char.(s.[n - 1] = 'A')) then "T" else "A"

let () = printf "%s\n%!" ans
