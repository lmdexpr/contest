open Core
open Scanf

let s = scanf " %s" Fn.id

let ans = match s with 
  | "red" -> "SSS"
  | "blue" -> "FFF"
  | "green" -> "MMM" 
  | _ -> "Unknown"

let () = printf "%s\n%!" ans
