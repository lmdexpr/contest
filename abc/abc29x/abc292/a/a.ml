open Core
open Scanf

let s = scanf "%s" ident

let ans = String.map s ~f:Char.uppercase

let () = printf "%s\n%!" ans
