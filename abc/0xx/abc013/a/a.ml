open Core
open Scanf

let x = scanf "%c" ident

let ans = Char.(to_int x - to_int 'A' + 1)

let () = printf "%d\n%!" ans
