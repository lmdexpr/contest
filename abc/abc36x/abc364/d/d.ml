open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let () =
  Array.sort a ~compare

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let lower_bound, upper_bound =
  let arr_binsearch meth x = 
    Array.binary_search a ~compare meth x |> Option.value ~default:n
  in
  arr_binsearch `First_greater_than_or_equal_to,
  arr_binsearch `First_strictly_greater_than

let () =
  for _ = 1 to q do
    let b, k = scanf " %d %d" Tuple2.create in
    let ok x = upper_bound (b + x) - lower_bound (b - x) >= k in
    binsearch ~ok (-1) 200_000_010 |> printf "%d\n"
  done
