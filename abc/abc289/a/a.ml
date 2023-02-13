open Core
open Scanf

let s = scanf "%s" ident

let () =
  String.iter s ~f:(function '0' -> printf "1" | _ -> printf "0");
  printf "\n%!"
