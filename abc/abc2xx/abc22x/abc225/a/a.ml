open Core
open Scanf

let s = scanf "%s" ident
let s = String.to_array s |> Char.Set.of_array

let ans =
  match Char.Set.length s with
  | 1 -> 1
  | 2 -> 3
  | 3 -> 6
  | _ -> assert false

let () = printf "%d\n%!" ans
