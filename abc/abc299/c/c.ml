open Core
open Scanf

let n = scanf "%d" ident
let s = scanf " %s" ident

let ans =
  String.split_on_chars ~on:['-'] s
  |> List.map ~f:String.length
  |> List.max_elt ~compare
  |> Option.map ~f:(fun x -> if x = n || x = 0 then -1 else x)
  |> Option.value ~default:(-1)

let () = printf "%d\n%!" ans
