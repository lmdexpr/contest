open Core
open Scanf

let n, l, r = scanf "%d %d %d" Tuple3.create

let () = 
  for _ = 1 to n do
    let a = scanf " %d" Fn.id in
    if a < l then printf "%d " l
    else if a > r then printf "%d " r
    else printf "%d " a
  done;
  printf "\n"
