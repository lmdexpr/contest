open Core
open Scanf

let n = scanf "%Ld" Fn.id

let rec log2 ?(k=0) ?(acc=1L) n = 
  let open Int64 in
  let acc = acc * 2L in
  if n < acc then k
  else 
    log2 ~k:Int.(k+1) ~acc n

let ans = log2 n

let () = printf "%d\n%!" ans
