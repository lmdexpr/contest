open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)
let t = Array.init m ~f:(fun _ -> scanf " %s" Fn.id) |> String.Set.of_array

let () = 
  Array.iter s ~f:(fun x -> if Set.mem t x then printf "Yes\n" else printf "No\n")
