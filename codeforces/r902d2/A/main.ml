open Scanf
open Printf

let id x = x

let solve _n a =
  printf "%d\n" (- Array.fold_left (+) 0 a)

let t = scanf "%d" id
let () =
  for _ = 1 to t do
    let n = scanf " %d" id in
    let a = Array.init (n-1) (fun _ -> scanf " %d" id) in
    solve n a
  done
