open Core
open Scanf

let s1 = scanf "%s" Fn.id
let s2 = scanf " %s" Fn.id
let s3 = scanf " %s" Fn.id

let t = scanf " %s" Fn.id

let () =
  String.iter t ~f:(function
    | '1' -> printf "%s" s1
    | '2' -> printf "%s" s2
    | '3' -> printf "%s" s3
    | _ -> assert false
  );
  printf "\n"

