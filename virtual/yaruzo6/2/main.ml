open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create
let c, d = scanf " %d %d" Tuple2.create

let k = c * d - b

let () =
  if k = 0 then (printf "-1\n%!"; exit 0)

let k = float a /. float k |> Float.round_up |> Float.to_int

let () = printf "%d\n%!" @@ max (-1) k
