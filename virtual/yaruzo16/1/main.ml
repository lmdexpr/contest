open Core
open Scanf

let c = scanf "%c" Fn.id

let is_right c = match c with
  | 'O' | 'P' | 'K' | 'L' -> true
  | _ -> false

let ans = if is_right c then "Right" else "Left"

let () = printf "%s\n%!" ans
