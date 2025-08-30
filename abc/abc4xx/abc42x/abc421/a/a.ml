open Core
open Scanf

let n = scanf " %d" Fn.id

let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let x = scanf " %d" Int.pred
let y = scanf " %s" Fn.id

let yes = String.equal s.(x) y

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
