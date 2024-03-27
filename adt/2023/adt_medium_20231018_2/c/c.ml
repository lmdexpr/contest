open Core
open Scanf

let k = scanf "%d" Fn.id
let a, b = scanf " %s %s" Tuple2.create

let notation k =
  String.fold ~init:0 ~f:(fun acc c ->
    let c = Char.(to_int c - to_int '0') in
    acc * k + c
  )

let ans = Int64.(of_int (notation k a) * of_int (notation k b))

let () = printf "%Ld\n%!" ans
