open Core
open Scanf

let n, t = scanf "%d %Ld" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let t = ref Int64.(t % Array.fold a ~init:0L ~f:(+))

let () =
  for i = 0 to n - 1 do
    if Int64.(!t < a.(i)) then (
      printf "%d %Ld\n" (i+1) !t;
      exit 0
    ) else
      t := Int64.(!t - a.(i))
  done
