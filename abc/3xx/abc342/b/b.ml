open Core
open Scanf

let n = scanf "%d" Fn.id

let order = Array.create ~len:(n+1) 0
let () =
  for i = 1 to n do
    order.(scanf " %d" Fn.id) <- i
  done

let q = scanf " %d" Fn.id
let () =
  for _ = 1 to q do
    let a = scanf " %d" Fn.id in
    let b = scanf " %d" Fn.id in
    if order.(a) < order.(b) then
      printf "%d\n%!" a
    else
      printf "%d\n%!" b
  done
