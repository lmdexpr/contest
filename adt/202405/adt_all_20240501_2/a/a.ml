open Core
open Scanf

let s = scanf "%s" Fn.id
let a, b = scanf " %d %d" Tuple2.create

let ans = 
  let s = String.to_array s in
  let tmp = s.(a - 1) in
  s.(a - 1) <- s.(b - 1);
  s.(b - 1) <- tmp;
  String.of_char_list (Array.to_list s)

let () = printf "%s\n%!" ans
