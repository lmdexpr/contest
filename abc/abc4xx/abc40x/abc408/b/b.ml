let sort_uniq = List.sort_uniq

open Core
open Scanf

let n = scanf " %d" Fn.id
let a = List.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = sort_uniq Int.compare a

let () =
  printf "%d\n%!" (List.length ans);
  List.iter ~f:(Printf.printf "%d ") ans;
  printf "\n%!";
