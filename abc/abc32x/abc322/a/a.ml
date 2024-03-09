open Core
open Scanf

let _n = scanf "%d" Fn.id
let s = scanf " %s" Fn.id

let ans = String.substr_index s ~pattern:"ABC" |> Option.value_map ~default:(-1) ~f:Int.succ

let () = printf "%d\n%!" ans
