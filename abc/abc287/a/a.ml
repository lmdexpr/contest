open Core
open Scanf

let n = scanf "%d" ident

let s = List.init n ~f:(fun _ -> scanf " %s" ident)

let yes = List.count s ~f:String.((=) "For") * 2 > n
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
