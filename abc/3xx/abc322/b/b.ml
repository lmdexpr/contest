open Core
open Scanf

let _ = scanf "%d %d" Tuple2.create
let s = scanf " %s" Fn.id
let t = scanf " %s" Fn.id

let is_prefix = String.is_prefix t ~prefix:s
let is_suffix = String.is_suffix t ~suffix:s

let ans =
  match is_prefix, is_suffix with
  | true, true   -> 0
  | true, false  -> 1
  | false, true  -> 2
  | false, false -> 3

let () = printf "%d\n%!" ans
