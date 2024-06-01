open Core
open Scanf

let n = scanf "%d" Fn.id
let l, r = scanf " %d %d" Tuple2.create

let () =
  for i = 1 to l - 1 do
    printf "%d " i
  done;
  for i = r downto l do
    printf "%d " i
  done;
  for i = r + 1 to n do
    printf "%d " i
  done;
  printf "\n"
