open Core
open Scanf

let _ = scanf " %d" Fn.id
let s = scanf " %s" @@ String.split_on_chars ~on:(['/'])

let yes =
  match s with
  | [one; two] -> 
    String.length one = String.length two &&
    String.for_all one ~f:Char.((=) '1') &&
    String.for_all two ~f:Char.((=) '2')
  | _ -> 
    false

let ans = if yes then "Yes" else "No"
let () =
  printf "%s\n%!" ans

