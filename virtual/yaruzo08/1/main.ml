open Core
open Scanf

let s = scanf "%s" String.to_array
let a, b = scanf " %d %d" Tuple2.create

let () =
  Array.swap s (a - 1) (b - 1);
  Array.iter s ~f:(printf "%c");
  printf "\n"
