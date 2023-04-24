open Core
open Scanf

let _ = scanf "%d" ident
let s = scanf " %s" ident

let bar1 = String.index s '|' |> Option.value ~default:0
let star = String.index s '*' |> Option.value ~default:0
let bar2 = String.index_from s (bar1+1) '|' |> Option.value ~default:0

let ans = if bar1 < star && star < bar2 then "in" else "out"

let () = printf "%s\n%!" ans
