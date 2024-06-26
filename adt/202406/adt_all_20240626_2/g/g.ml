open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let m = scanf " %d" Fn.id
let b = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let x = scanf " %d" Fn.id

let ans = 0

let () = printf "%d\n%!" ans
