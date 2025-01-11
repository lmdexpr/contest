open Core
open Scanf

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = binsearch (n / 2 + 1) 0 ~ok:(fun k ->
  Iter.(0 -- (k - 1))
  |> Iter.for_all (fun i -> a.(i) * 2 <= a.(n - k + i))
)

let () = printf "%d\n%!" ans
