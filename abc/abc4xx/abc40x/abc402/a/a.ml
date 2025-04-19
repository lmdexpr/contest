open Core
open Scanf

let s = scanf " %s" Fn.id

let () = 
  String.iter s ~f:(fun c ->
    if Char.is_uppercase c then
      printf "%c" c
  );
  printf "\n";
