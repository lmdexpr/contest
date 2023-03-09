open Core
open Scanf

let n = scanf "%d" ident
let t = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ans = Array.min_elt t ~compare |> Option.value ~default:0

let () = printf "%d\n%!" ans
