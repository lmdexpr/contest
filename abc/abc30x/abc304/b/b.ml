open Core
open Scanf

let n = scanf "%s" ident

let size = String.length n - 1

let () =
  for i = 0 to min 2 size do
    printf "%c" n.[i]
  done;
  for _ = min 2 size + 1 to size do
    printf "0"
  done;
  printf "\n"
