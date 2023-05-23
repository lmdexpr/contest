open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ans = 0

let () = printf "%d\n%!" ans
