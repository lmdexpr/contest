open Core
open Scanf

let n = scanf "%Ld" Fn.id

let ans =
  let open Int64 in
  if n > 0L then
    n / 10L + if n % 10L =  0L then 0L else 1L
  else
    n / 10L

let () = printf "%Ld\n" ans
