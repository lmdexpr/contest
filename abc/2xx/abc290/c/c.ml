open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)
let () = Array.sort a ~compare

let a = Iter.of_array a |> Iter.uniq ~eq:(=) |> Iter.take k |> Iter.to_array
let len = Array.length a

let rec solve ?(acc=0) i =
  if i >= len then acc
  else if acc = a.(i) then solve ~acc:(acc + 1) (i + 1)
  else
    acc

let ans = solve 0

let () = printf "%d\n%!" ans
