open Core

let n = Scanf.scanf "%d" ident
let s = " " ^ Scanf.scanf " %s" ident

let rec solve ?(l=1) i =
  if l + i > n || Char.(s.[l] = s.[l + i]) then l - 1
  else
    solve ~l:(l + 1) i

let () =
  for i = 1 to n - 1 do
    printf "%d\n%!" @@ solve i
  done
