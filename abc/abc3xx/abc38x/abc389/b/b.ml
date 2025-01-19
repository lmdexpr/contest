open Core
open Scanf

let x = scanf " %Ld" Fn.id

let inverse_fact x =
  let open Int64 in
  let rec loop n x =
    let x = x / n in
    if x = 1L then n
    else
      loop (n + 1L) x
  in
  loop 1L x

let ans = inverse_fact x

let () = printf "%Ld\n%!" ans
