open Core
open Scanf

let _, s = scanf "%d %s" Tuple2.create

let ans =
  match String.substr_index s ~pattern:"MM", String.substr_index s ~pattern:"FF" with
  | Some _, _ | _, Some _ -> "No"
  | _ -> "Yes"

let () = printf "%s\n%!" ans
