open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

let x = Array.create ~len:m 0
let y = Array.create ~len:m 0
let z = Array.create ~len:m 0

let () =
  for i = 0 to m - 1 do
    x.(i) <- scanf " %d" Fn.id;
    y.(i) <- scanf " %d" Fn.id;
    z.(i) <- scanf " %d" Fn.id;
  done

let a = Array.create ~len:(n+1) 0

let () =
  for i = 0 to m - 1 do
    a.(x.(i)) <- z.(i) lxor a.(y.(i))
  done
