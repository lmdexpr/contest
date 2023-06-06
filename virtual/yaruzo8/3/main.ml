open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let max a = Option.value_exn (Array.max_elt a ~compare)
let min a = Option.value_exn (Array.min_elt a ~compare)

let ans = max a - min a

let () = printf "%d\n%!" ans
