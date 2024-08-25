open Core
open Scanf

let n = scanf "%d" Fn.id

let () =
  for _ = 1 to n do
    printf "%d" n
  done;
  printf "\n"
