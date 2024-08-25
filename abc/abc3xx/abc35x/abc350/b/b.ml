open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let t = Array.create ~len:n true

let () =
  for _ = 1 to q do
    let q = scanf " %d" (fun x -> x - 1) in
    t.(q) <- not t.(q)
  done

let ans = Array.count t ~f:Fn.id

let () = printf "%d\n%!" ans
