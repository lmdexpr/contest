open Core
open Scanf

let n = scanf " %d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let () =
  Array.sort s ~compare:(fun s t -> compare (String.length s) (String.length t));
  Array.iter s ~f:(fun s -> printf "%s%!" s);
  printf "\n%!"
