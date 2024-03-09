open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" @@ fun x -> x - 1)

let same = Array.counti a ~f:(=)

let reverse = Array.counti a ~f:(fun i x -> x > i && a.(x) = i)

let ans = same * (same - 1) / 2 + reverse

let () = printf "%d\n%!" ans
