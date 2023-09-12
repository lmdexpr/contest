open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let s = scanf " %s" ident

let alp = Array.create ~len:26 0
let () =
  String.iter s ~f:(fun c ->
      let i = Char.to_int c - Char.to_int 'a' in
      alp.(i) <- alp.(i) + 1
    )

let f t =
  String.length t

let ans = 0

let () = printf "%d\n%!" ans
