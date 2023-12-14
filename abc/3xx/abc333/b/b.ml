open Core
open Scanf

let a_to_i c = Char.to_int c - Char.to_int 'A'

let s = scanf "%s" Fn.id
let s1, s2 = a_to_i s.[0], a_to_i s.[1]
let s1, s2 = min s1 s2, max s1 s2
let s = s1 + 1 = s2 || (s1 = 0 && s2 = 4)

let t = scanf " %s" Fn.id
let t1, t2 = a_to_i t.[0], a_to_i t.[1]
let t1, t2 = min t1 t2, max t1 t2
let t = t1 + 1 = t2 || (t1 = 0 && t2 = 4)

let yes = Bool.(s = t)

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
