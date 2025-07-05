open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes = Array.sum (module Int) a ~f:Fn.id <= m

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
