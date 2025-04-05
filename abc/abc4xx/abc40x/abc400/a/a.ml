open Core
open Scanf

let a = scanf " %d" Fn.id

let ans =
  if 400 % a <> 0 then -1
  else
    400 / a

let () = printf "%d\n%!" ans
