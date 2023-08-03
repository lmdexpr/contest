open Core
open Scanf

let n = scanf "%d" Fn.id

let p = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let m = Array.max_elt p ~compare |> Option.value ~default:0

let pm = Array.filter p ~f:((=) m) |> Array.length

let ans = m - p.(0)
let ans = if ans = 0 && pm = 1 then 0 else (ans + 1)

let () = printf "%d\n%!" ans
