open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  for i = 0 to n - 2 do
    let s, t = scanf " %d %d" Tuple2.create in
    a.(i + 1) <- a.(i + 1) + (a.(i) / s) * t
  done

let () = printf "%d\n%!" a.(n - 1)
