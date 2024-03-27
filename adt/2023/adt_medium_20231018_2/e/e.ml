open Core
open Scanf

let s = scanf "%s" Fn.id

let yes s =
  let t = String.rev s in
  let x = String.take_while s ~f:(Char.(=) 'a') |> String.length in
  let y = String.take_while t ~f:(Char.(=) 'a') |> String.length in
  let s = String.drop_prefix s x in
  let s = String.drop_suffix s y in
  x = String.length s ||
  x <= y &&
  String.(s = rev s)

let ans = if yes s then "Yes" else "No"
let () = printf "%s\n%!" ans
