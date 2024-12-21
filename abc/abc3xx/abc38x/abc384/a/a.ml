open Core
open Scanf

let n, c1, c2 = scanf "%d %c %c" Tuple3.create
let s         = scanf " %s" Fn.id

let () =
  for i = 0 to n - 1 do
    printf "%c" @@
    if Char.(s.[i] <> c1) then
      c2
    else
      s.[i]
  done;
  printf "\n"
