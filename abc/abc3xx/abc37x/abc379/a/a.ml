open Core
open Scanf

let a, b, c = scanf "%c%c%c\n" Tuple3.create

let () =
  printf "%c%c%c %c%c%c\n" b c a c a b
