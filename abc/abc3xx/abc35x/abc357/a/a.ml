open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let h = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let _ = 
  Array.foldi h ~init:m ~f:(fun i m h ->
    let m = m - h in
    if m < 0 then (printf "%d\n" i; exit 0);
    m
  )

let () =
  printf "%d\n" n
