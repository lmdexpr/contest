open Core
open Scanf

let n = scanf "%d" ident

let () = 
  for _ = 1 to n do
    scanf " %d %d" (+) |> printf "%d\n"
  done
