open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ ->
  let l = scanf " %d" Fn.id in
  Array.init l ~f:(fun _ -> scanf " %d" Fn.id)
)

let () = 
  for _ = 1 to q do
    let i, j = scanf " %d %d" Tuple2.create in
    printf "%d\n" a.(i - 1).(j - 1)
  done
