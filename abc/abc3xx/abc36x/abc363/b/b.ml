open Core
open Scanf

let n, t, p = scanf "%d %d %d" Tuple3.create

let l  = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let () = Array.sort l ~compare:Int.descending

let ans = max 0 @@ t - l.(p - 1)

let () = printf "%d\n%!" ans
