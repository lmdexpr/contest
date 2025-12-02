open Core
open Scanf

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans = binsearch 0 1_000_000_000
  ~ok:(fun x ->
    k <= Array.sum (module Int) a ~f:(fun a -> x / a)
  )

let () = printf "%d\n%!" ans
