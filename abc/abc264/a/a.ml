open Core
open Scanf

let l, r = scanf "%d %d" Tuple2.create

let () =
  for i = l to r do
    printf "%c" "atcoder".[i - 1]
  done;
  printf "\n%!"
