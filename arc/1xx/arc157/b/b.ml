open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let s = scanf " %s" ident |> String.to_array |> Array.map ~f:Char.((=) 'Y')

let x = n - Array.count s ~f:ident

let ans = 0

let () = printf "%d\n%!" ans
