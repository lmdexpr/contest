open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)
let even, odd = Array.partition_tf a ~f:(fun a -> a % 2 = 0)
let () =
  let compare a b = - compare a b in
  Array.sort ~compare even;
  Array.sort ~compare odd

let solve a = if Array.length a < 2 then -1 else a.(0) + a.(1)

let ans = max (solve even) (solve odd)

let () = printf "%d\n%!" ans
