open Core
open Scanf

let n = scanf "%d" Fn.id
let names = Array.init n ~f:(fun _ -> scanf " %s %s" @@ fun s t -> s ^ " " ^ t) |> String.Set.of_array

let ans = if Set.length names = n then "No" else "Yes"

let () = printf "%s\n%!" ans
