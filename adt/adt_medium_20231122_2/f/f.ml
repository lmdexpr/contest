open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let () = Array.sort a ~compare; Array.sort b ~compare

let rec solve ?(acc=Int.max_value) i j =
  if i >= n || j >= m then acc
  else
    let diff = abs (a.(i) - b.(j)) in
    let acc  = min acc diff in
    if a.(i) < b.(j) then
      solve ~acc (i + 1) j
    else 
      solve ~acc i (j + 1)

let ans = solve 0 0

let () = printf "%d\n%!" ans
