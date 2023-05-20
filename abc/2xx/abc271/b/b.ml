open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(const @@ Array.create ~len:1 0)
let () =
  for i = 0 to n - 1 do
    let l = scanf " %d" ident in
    a.(i) <- Array.init l ~f:(fun _ -> scanf " %d" ident)
  done

let () =
  for _ = 1 to q do
    let s, t = scanf " %d %d" Tuple2.create in
    printf "%d\n%!" a.(s - 1).(t - 1)
  done
