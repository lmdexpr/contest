open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let max a = Array.max_elt a ~compare |> Option.value_exn

let ans = max a + max b

let () = printf "%d\n%!" ans
