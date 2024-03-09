open Core
open Scanf

let n = scanf "%d" Fn.id

let fs = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)
let () = Array.sort fs ~compare:(fun (_, s1) (_, s2) -> Int.descending s1 s2)

let one = 
  let (_, s) = fs.(0) and (_, t) = fs.(1) in
  let s, t = max s t, min s t in
  s + t / 2

let two =
  let (f, s) = fs.(0) in
  match Array.find fs ~f:(fun (f', _) -> f' <> f) with
  | None        -> 0
  | Some (_, t) -> s + t

let ans = max one two

let () = printf "%d\n%!" ans
