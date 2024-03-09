open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let ans =
  Array.fold a ~init:0L ~f:Int64.(fun acc x -> max (acc + x) 0L)

let () = printf "%Ld\n%!" ans
