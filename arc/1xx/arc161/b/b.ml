open Core
open Scanf

let t = scanf "%d" ident

let rec solve n popcount =
  if popcount = 3 then n
  else if popcount < 3 then
    solve Int64.(n - 1L) Int64.(popcount @@ n - 1L)
  else
    solve Int64.(n lxor (n land -n)) (popcount - 1)

let solve n =
  if Int64.(n < 7L) then -1L
  else
    solve n Int64.(popcount n)

let () =
  for _ = 1 to t do
    scanf " %Ld" solve |> printf "%Ld\n"
  done
