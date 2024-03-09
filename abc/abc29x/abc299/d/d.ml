open Core
open Scanf

let n = scanf "%d" ident

let ask k =
  printf "? %d\n%!" k;
  scanf " %d" ident

let rec binsearch left right =
  if abs (right - left) <= 1 then left
  else
    let mid = (right + left) / 2 in
    let left, right = if ask mid = 1 then left, mid else mid, right in
    binsearch left right

let p = binsearch 1 n

let () = printf "! %d\n%!" p
