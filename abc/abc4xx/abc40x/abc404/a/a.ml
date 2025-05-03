open Core
open Scanf

let s = scanf " %s" String.to_list |> Char.Set.of_list

let ans =
  "abcdefghijklmnopqrstuvwxyz"
  |> String.find ~f:(fun c -> not (Set.mem s c))
  |> Option.value_exn

let () = printf "%c\n%!" ans
