open Scanf
open Printf

let id x = x

let n = scanf "%d" id

let _a = Array.init n (fun _ -> scanf " %d" id)

let ans = 0

let () = printf "%d\n%!" ans
