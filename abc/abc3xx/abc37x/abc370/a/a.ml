open Core
open Scanf

let l, r = scanf "%d %d" Tuple2.create

let () =
  if l = r then printf "Invalid\n"
  else if l = 1 then
    printf "Yes\n"
  else
    printf "No\n"
