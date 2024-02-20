open Core
open Scanf

let _n = scanf "%d" Fn.id
let  s = scanf " %s" Fn.id

let ans = String.substr_replace_all s ~pattern:"na" ~with_:"nya"

let () = printf "%s\n%!" ans
