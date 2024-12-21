open Core
open Scanf

let n, s = scanf "%d %Ld" Tuple2.create
let a    = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let sum = Array.fold a ~init:0L ~f:Int64.(+)

let s = Int64.(s % sum)

let rec yes acc l r =
  if n < l || 2 * n < r then false
  else if Int64.(acc < s) then
    let r = r + 1 in
    yes Int64.(acc + a.(Int.(r % n))) l r
  else if Int64.(s < acc) then
    yes Int64.(acc - a.(Int.(l % n))) (l + 1) r
  else
    true

let ans = if yes a.(0) 0 0 then "Yes" else "No"

let () = printf "%s\n%!" ans
