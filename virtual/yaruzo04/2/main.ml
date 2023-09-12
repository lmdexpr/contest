open Core
open Scanf

let n = scanf "%s" ident

let sn  =
  String.to_array n
  |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0')
  |> Array.sum (module Int) ~f:ident
let n   = Int.of_string n
let yes = n % sn = 0

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
