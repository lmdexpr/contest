open Core
open Scanf

let to_i c = Char.to_int c - Char.to_int 'A'

let s = Array.create ~len:26 0
let () =
  scanf "%s" (String.iteri ~f:(fun i c ->
    s.(to_i c - to_i 'A') <- i
  ))

let ans, _ =
  String.fold "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ~init:(0,'A') ~f:(fun (acc, p) c ->
    acc + abs (s.(to_i c) - s.(to_i p)), c
  )

let () = printf "%d\n%!" ans
