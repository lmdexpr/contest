open Core
open Scanf

let n  = scanf "%s" String.to_array |> Array.map ~f:(fun c -> Char.(to_int c - to_int '0'))
let () = Array.sort n ~compare:Int.descending

let len = Array.length n

let ans = 0

let () = printf "%d\n%!" ans
