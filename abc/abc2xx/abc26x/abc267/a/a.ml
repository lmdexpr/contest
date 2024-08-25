open Core
open Scanf

let ans =
  match scanf "%s" ident with
  | "Monday"    -> 5
  | "Tuesday"   -> 4
  | "Wednesday" -> 3
  | "Thursday"  -> 2
  | _           -> 1

let () = printf "%d\n%!" ans
