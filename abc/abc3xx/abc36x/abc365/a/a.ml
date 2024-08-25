open Core
open Scanf

let y = scanf "%d" Fn.id

let ans = 
  if y % 4 <> 0 then 365
  else if y % 100 <> 0 then 366
  else if y % 400 <> 0 then 365
  else 366

let () = printf "%d\n%!" ans
