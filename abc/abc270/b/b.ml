open Core
open Scanf

let x, y, z = scanf "%d %d %d" Tuple3.create

let ans =
  let x, y, z = if y < 0 then -x, -y, -z else x, y, z in
  if x < y then abs x
  else if z > y then -1
  else
    abs z + abs (x - z)

let () = printf "%d\n%!" ans
