open Core
open Scanf

let n, p, q = scanf "%d %d %d" Tuple3.create

let d = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let d = Array.min_elt d ~compare |> Option.value ~default:0

let ans = min p (q + d)

let () = printf "%d\n%!" ans

