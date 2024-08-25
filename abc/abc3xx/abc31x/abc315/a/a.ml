open Core
open Scanf

let s = scanf "%s" Fn.id

let ans = String.filter s ~f:(function
  | 'a' | 'i' | 'u' | 'e' | 'o' -> false
  | _ -> true 
)

let () = printf "%s\n%!" ans
