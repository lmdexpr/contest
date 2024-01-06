open Core
open Scanf

let s = scanf "%s" Fn.id

let () =
  for i = 0 to String.length s - 2 do
    printf "%c" s.[i]
  done;
  printf "4\n%!"
