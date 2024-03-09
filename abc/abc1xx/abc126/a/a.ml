open Core
open Scanf

let _n, k = scanf "%d %d" Tuple2.create
let s = scanf " %s" String.to_array

let () =
  s.(k - 1) <- Char.lowercase s.(k - 1);
  Array.iter s ~f:(printf "%c");
  printf "\n%!"
