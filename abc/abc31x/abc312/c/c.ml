open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  Array.sort a ~compare;
  Array.sort b ~compare

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ans =
  binsearch 0 1_000_000_001 ~ok:(fun x ->
    let arr_bsearch ~default a mtd x =
      Option.value ~default (Array.binary_search ~compare a mtd x)
    in
    let a = arr_bsearch a `Last_less_than_or_equal_to x ~default:(-1) + 1 in
    let b = m - arr_bsearch b `First_greater_than_or_equal_to  x ~default:m in
    a >= b
  )

let () = printf "%d\n%!" ans
