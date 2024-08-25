open Core
open Scanf

let r, g, b = scanf "%d %d %d" Tuple3.create
let c = scanf " %s" Fn.id

let ans =
  match c with
  | "Red"   -> min g b
  | "Green" -> min r b
  | "Blue"  -> min r g
  | _       -> assert false

let () = printf "%d\n%!" ans
