open Core
open Scanf

let x, y, z = scanf "%d %d %d" Tuple3.create

let x, y, z = if y < 0 then -x, -y, -z else x, y, z

let ans =
  if x < y then abs x
  else
    if y < z then -1
    else
      if 0 < z then abs x
      else 
        abs z * 2 + abs x

let () = printf "%d\n%!" ans
