open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let p = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let q = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes = 
  Array.exists p ~f:(fun p ->
    Array.exists q ~f:(fun q -> p + q = k)
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
