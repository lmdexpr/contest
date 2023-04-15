open Core
open Scanf

let _ = scanf "%d" ident
let s = scanf " %s" ident

let yes = String.count s ~f:Char.((=) 'o') > 0 && String.count s ~f:Char.((=) 'x') = 0
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
