open Core
open Scanf

let s = scanf "%s" String.to_array

let ans = Array.findi_exn s ~f:Char.(fun _ c -> is_uppercase c) |> Tuple2.get1
let ans = ans + 1

let () = printf "%d\n%!" ans
