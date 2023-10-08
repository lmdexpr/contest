open Scanf
open Printf

let id x = x

let solve () =
  let n = scanf " %d" id in
  let _a = Array.init n (fun _ -> scanf " %d" id) in
  let ans = 0 in
  printf "%d\n" ans

let () =
  for _ = 1 to scanf "%d" id do
    solve ()
  done
