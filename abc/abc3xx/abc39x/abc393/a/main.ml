open Core
open Scanf

let s1, s2 = scanf " %s %s" Tuple2.create

let ans =
  match s1, s2 with
  | "sick", "sick" -> 1
  | "sick", "fine" -> 2
  | "fine", "sick" -> 3
  | _              -> 4

let () = printf "%d\n%!" ans
