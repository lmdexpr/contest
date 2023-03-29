open Core
open Scanf

let y = scanf "%d" ident

let ans = y + match y % 4 with
  | 2 -> 0
  | 1 -> 1
  | 0 -> 2
  | _ -> 3

let () = printf "%d\n%!" ans
