open Core
open Scanf

let s = scanf "%s" Fn.id

let ans = String.filter s ~f:Char.((<>) '.')

let () = printf "%s\n%!" ans
