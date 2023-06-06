open Core
open Scanf

let solve _n _s =
  0

let () =
  for _ = 1 to scanf "%d" ident do
    let n = scanf " %d" ident in
    let s = scanf " %s" ident in
    solve n s |> printf "%d\n"
  done
