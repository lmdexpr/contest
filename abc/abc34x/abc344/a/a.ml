open Core
open Scanf

let s = scanf "%s" Fn.id

let s = String.split s ~on:'|'
let s = match s with
  | [a; _; b] -> a ^ b
  | s         -> String.concat s

let () =
  printf "%s\n" s
