open Core
open Scanf

let s = scanf " %d" Fn.id
let a = scanf " %d" Fn.id
let b = scanf " %d" Fn.id
let x = scanf " %d" Fn.id

let c = x / (a + b)
let d = x % (a + b)

let ans = (c * a + min a d) * s

let () = printf "%d\n%!" ans
