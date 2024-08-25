open Core
open Scanf

let s = scanf "%s" String.to_array

let first =
  let b = Array.filter_mapi s ~f:(fun i c -> if Char.(c = 'B') then Some i else None) in
  b.(0) % 2 <> b.(1) % 2

let second =
  let k = Array.find_mapi_exn s ~f:(fun i c -> if Char.(c = 'K') then Some i else None) in
  let r = Array.filter_mapi s ~f:(fun i c -> if Char.(c = 'R') then Some i else None) in
  r.(0) < k && k < r.(1)

let yes = first && second
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
