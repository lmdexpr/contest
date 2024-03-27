open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Int.pred)

let same = Int64.of_int @@ Array.counti a ~f:(=)
let reve = Int64.of_int @@ Array.counti a ~f:(fun i j -> i < j && a.(j) = i)

let ans = Int64.(same * (same - 1L) / 2L + reve)

let () = printf "%Ld\n%!" ans
