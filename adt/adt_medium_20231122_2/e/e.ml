open Core
open Scanf

let n = scanf "%d" Fn.id

let () =
  printf "%d\n%!" (n + 1);
  for _ = 1 to n do
    let a = scanf " %d" Fn.id in
    printf "%d\n%!" (2 * n + 2 - a)
  done
