open Core
open Scanf

let x = scanf "%Ld" Fn.id

let ans = Int64.(
    if x > 0L then 
      x / 10L + if x % 10L = 0L then 0L else 1L
    else 
      x / 10L
  )

let () = printf "%Ld\n%!" ans
