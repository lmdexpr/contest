open Scanf
open Printf

let id x = x

let solve () =
  let n, m, k = scanf " %d %d %d" @@ fun n m k -> n, m, k in
  let ans = 
    match k with
    | 1 -> 1
    | 2 -> min m (n + m / n - 1)
    | 3 -> max 0 (m - n - m / n + 1)
    | _ -> 0
  in
  printf "%d\n" ans

let () =
  for _ = 1 to scanf "%d" id do
    solve ()
  done
