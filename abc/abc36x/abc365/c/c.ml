open Core
open Scanf

let n, m = scanf "%d %Ld" Tuple2.create

let a  = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)
let () = Array.sort a ~compare:Int64.descending

open Int64
let rec binsearch ~ok left right =
  if abs (right - left) <= 1L then right
  else
    let mid = (right + left) / 2L in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans = 
  binsearch (m + 1L) 0L ~ok:(fun x ->
    Array.fold a ~init:0L ~f:(fun acc a -> acc + min a x) <= m
  )

let () =
  if ans = m then
    printf "infinite\n"
  else
    printf "%Ld\n%!" ans
