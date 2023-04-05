open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let () =
  Array.iter a ~f:(fun a -> if a <> x then printf "%d " a);
  printf "\n%!"
