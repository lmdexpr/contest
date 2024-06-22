open Core
open Scanf

let sx, sy = scanf "%Ld %Ld" Tuple2.create
let tx, ty = scanf " %Ld %Ld" Tuple2.create

open Int64

let is_even x = x % 2L = 0L

let sx = sx - if is_even @@ sx + sy then 0L else 1L
let tx = tx - if is_even @@ tx + ty then 0L else 1L

let dx = abs @@ tx - sx
let dy = abs @@ ty - sy

let ans =
  if dx < dy then dy
  else
    let sx = sx + if tx < sx then -dy else dy in
    dy + abs (tx - sx) / 2L

let () = printf "%Ld\n%!" ans
