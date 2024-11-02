open Core
open Scanf

let n = scanf "%d" Fn.id

let qr = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let q = scanf " %d" Fn.id
let () =
  for _ = 1 to q do
    let t, d = scanf " %d %d" @@ fun t d -> t - 1, d in
    let q, r = qr.(t) in
    let ans = d + (r - d % q + q) % q in
    printf "%d\n%!" ans
  done
