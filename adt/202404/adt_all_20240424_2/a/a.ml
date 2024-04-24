open Core
open Scanf

let s = scanf "%s" String.to_list |> Char.Set.of_list

let ans =
  match Set.length s with
  | 3 -> 6
  | 2 -> 3
  | _ -> 1

let () = printf "%d\n%!" ans
