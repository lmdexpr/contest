open Core
open Scanf

let k = scanf "%d" ident
let a, b = scanf " %d %d" Tuple2.create

let ok = Iter.(a -- b) |> Iter.exists (fun x -> x % k = 0)
let ok = if ok then "OK" else "NG"

let () = printf "%s\n%!" ok
