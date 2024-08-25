open Core
open Scanf

let n = scanf "%d" Fn.id
let ps = Array.init n ~f:(fun _ -> scanf " %s %d" Tuple2.create)

let () = Array.sort ps ~compare:(fun (a, _) (b, _) -> String.compare a b)

let t = Array.fold ps ~init:0 ~f:(fun acc (_, x) -> acc + x)

let () = printf "%s\n%!" @@ fst ps.(t % n)
