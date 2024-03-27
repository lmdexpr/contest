open Core
open Scanf

let n = scanf  "%d" Fn.id
let s = " " ^ scanf " %s" Fn.id

let rec solve ?(l=1) i =
  if l + i > n || Char.(s.[l] = s.[l + i]) then l - 1
  else
    solve ~l:(l + 1) i

let () =
  for i = 1 to n - 1 do
    printf "%d\n%!" @@ solve i
  done
