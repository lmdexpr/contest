open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun i -> scanf " %d" Fn.id, i + 1)

let () = Array.sort a ~compare:(fun (x, _) (y, _) -> Int.descending x y)

let ans = a.(1) |> snd

let () = printf "%d\n%!" ans
