open Core
open Scanf

let n = scanf "%d" ident
let s, t = scanf " %s %s" Tuple2.create

let () =
  for i = 0 to n - 1 do
    printf "%c%c" s.[i] t.[i]
  done;
  printf "\n"
