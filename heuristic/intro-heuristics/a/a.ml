open Core
open Scanf

let d = scanf "%d" Fn.id
let c = Array.init 26 ~f:(fun _ -> scanf " %d" Fn.id)
let s = Array.init d ~f:(fun _ -> Array.init 26 ~f:(fun _ -> scanf " %d" Fn.id))

let ans = 0

let () = printf "%d\n%!" ans
