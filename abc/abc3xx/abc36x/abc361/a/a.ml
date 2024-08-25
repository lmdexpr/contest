open Core
open Scanf

let n, k, x = scanf "%d %d %d" Tuple3.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  for i = 0 to k - 1 do
    printf "%d " a.(i)
  done;
  printf "%d " x;
  for i = k to n - 1 do
    printf "%d " a.(i)
  done;
  printf "\n"
