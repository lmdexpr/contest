open Core
open Scanf

let s = scanf "%s" Fn.id

let ss = [ "ACE"; "BDF"; "CEG"; "DFA"; "EGB"; "FAC"; "GBD" ]

let yes = List.mem ss s ~equal:String.equal
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
