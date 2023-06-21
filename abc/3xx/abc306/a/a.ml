open Core
open Scanf

let n = scanf "%d" ident
let s = scanf " %s" ident

let () =
  for i = 0 to n - 1 do
    printf "%c%c" s.[i] s.[i]
  done;
  printf "\n%!"
