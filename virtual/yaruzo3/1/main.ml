open Core
open Scanf

let a, b, c, d = scanf "%d %d %d %d" @@ fun a b c d -> a, b, c, d

let () =
  printf "%d\n%!" @@ (a + b) * (c - d);
  printf "Takahashi\n%!"
