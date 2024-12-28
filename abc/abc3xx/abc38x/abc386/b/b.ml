open Core
open Scanf

let s = scanf "%s" Fn.id

let ans, _ = 
  String.fold s ~init:(0, false) ~f:(fun (cnt, prev) -> function
    | '0' when prev -> cnt, false
    | c             -> cnt + 1, Char.(c = '0')
  )

let () = printf "%d\n%!" ans
