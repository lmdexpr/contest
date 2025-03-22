open Core
open Scanf

let n = scanf " %d" Fn.id

let () =
  for i = 1 to n do
    if 
      (n % 2 = 0 && (i * 2 = n || (i - 1) * 2 = n)) || 
      (n % 2 = 1 && i * 2 - 1 = n) 
    then
      printf "="
    else
      printf "-"
  done;
  printf "\n"
