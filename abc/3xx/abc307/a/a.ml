open Core
open Scanf

let n = scanf "%d" ident

let () =
  for _ = 1 to n do
    Array.init 7 ~f:(fun _ -> scanf " %d" ident)
    |> Array.sum (module Int) ~f:ident
    |> printf "%d "
  done;
  printf "\n"

