open Core
open Scanf

let s = scanf "%s" Fn.id

let upper = String.count s ~f:Char.is_uppercase
let lower = String.count s ~f:Char.is_lowercase

let ans =
  if upper > lower then String.uppercase s
  else String.lowercase s

let () = printf "%s\n%!" ans
