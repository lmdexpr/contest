open Core
open Scanf

let s = scanf " %s" Fn.id

let ans =
  match s with
  | "N" -> "S"
  | "S" -> "N"
  | "W" -> "E"
  | "E" -> "W"
  | "NE" -> "SW"
  | "NW" -> "SE"
  | "SE" -> "NW"
  | "SW" -> "NE"
  | _ -> assert false

let () = printf "%s\n%!" ans
