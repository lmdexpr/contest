open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let rate = Array.init (n-1) ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let () =
  for i = 0 to n - 2 do
    let s, t = rate.(i) in
    a.(i + 1) <- a.(i + 1) + a.(i) / s * t
  done

let () = printf "%d\n%!" a.(n-1)
